# Multi-Tenant JWT Authentication with Webhook Mode

This guide explains how to implement multi-tenant JWT authentication in Hasura v3 using the webhook authentication mode, allowing admin users to dynamically configure JWT settings for their tenants.

## Overview

In this architecture, Hasura delegates authentication to an external webhook service that:
1. Manages JWT configurations for multiple tenants
2. Validates incoming JWTs using tenant-specific settings
3. Returns session variables to Hasura for authorization

## Architecture

```
┌─────────────┐     ┌─────────────────┐     ┌─────────────────┐     ┌──────────────┐
│   Client    │────>│   Hasura v3     │────>│  Auth Webhook   │────>│  Database    │
│ (with JWT)  │     │ (webhook mode)  │     │    Service      │     │ (JWT configs)│
└─────────────┘     └─────────────────┘     └─────────────────┘     └──────────────┘
                            │                                               │
                            │                 ┌─────────────────┐          │
                            └────────────────>│ Identity        │<─────────┘
                                             │ Providers       │
                                             │ (per tenant)    │
                                             └─────────────────┘
```

## Implementation Steps

### 1. Configure Hasura for Webhook Authentication

Create an auth configuration file (`auth_config.json`):

```json
{
  "version": "v3",
  "definition": {
    "mode": {
      "webhook": {
        "method": "POST",
        "url": {
          "value": "http://auth-webhook-service:3000/validate"
        },
        "customHeadersConfig": {
          "headers": {
            "forward": [
              "Authorization",
              "X-Tenant-ID",
              "X-Request-ID"
            ],
            "additional": {
              "X-Service": "hasura-v3"
            }
          }
        }
      }
    }
  }
}
```

Run Hasura with this configuration:

```bash
cargo run --bin engine -- \
    --metadata-path metadata.json \
    --authn-config-path auth_config.json \
    --port 3000
```

### 2. Database Schema for JWT Configurations

Create tables to store tenant-specific JWT configurations:

```sql
-- Tenants table
CREATE TABLE tenants (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name VARCHAR(255) NOT NULL,
    slug VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- JWT configurations per tenant
CREATE TABLE jwt_configs (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id UUID NOT NULL REFERENCES tenants(id) ON DELETE CASCADE,
    name VARCHAR(255) NOT NULL,
    algorithm VARCHAR(50) NOT NULL, -- HS256, RS256, etc.
    secret TEXT, -- For HMAC algorithms
    public_key TEXT, -- For RSA algorithms
    jwks_url TEXT, -- For JWKS endpoint
    issuer VARCHAR(255),
    audience VARCHAR(255),
    claims_namespace VARCHAR(255) DEFAULT 'https://hasura.io/jwt/claims',
    claims_format VARCHAR(50) DEFAULT 'json', -- json or stringified_json
    header_name VARCHAR(255) DEFAULT 'Authorization',
    header_prefix VARCHAR(50) DEFAULT 'Bearer',
    is_active BOOLEAN DEFAULT true,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(tenant_id, name)
);

-- Role mappings (optional - for role transformation)
CREATE TABLE role_mappings (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    jwt_config_id UUID NOT NULL REFERENCES jwt_configs(id) ON DELETE CASCADE,
    external_role VARCHAR(255) NOT NULL,
    hasura_role VARCHAR(255) NOT NULL,
    UNIQUE(jwt_config_id, external_role)
);

-- Audit log for authentication attempts
CREATE TABLE auth_audit_log (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id UUID REFERENCES tenants(id),
    jwt_config_id UUID REFERENCES jwt_configs(id),
    user_id VARCHAR(255),
    success BOOLEAN NOT NULL,
    error_message TEXT,
    ip_address INET,
    user_agent TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### 3. Webhook Service Implementation

Here's a complete example using Node.js/TypeScript:

```typescript
// src/types.ts
export interface JWTConfig {
  id: string;
  tenantId: string;
  algorithm: string;
  secret?: string;
  publicKey?: string;
  jwksUrl?: string;
  issuer?: string;
  audience?: string;
  claimsNamespace: string;
  claimsFormat: 'json' | 'stringified_json';
}

export interface HasuraWebhookRequest {
  headers: Record<string, string>;
}

export interface HasuraWebhookResponse {
  [key: string]: string | string[];
  'x-hasura-role': string;
  'x-hasura-user-id': string;
  'x-hasura-tenant-id': string;
}

// src/jwt-validator.ts
import jwt from 'jsonwebtoken';
import jwksClient from 'jwks-rsa';
import { JWTConfig } from './types';

export class JWTValidator {
  private jwksClients: Map<string, jwksClient.JwksClient> = new Map();

  async validateToken(token: string, config: JWTConfig): Promise<any> {
    const verifyOptions: jwt.VerifyOptions = {
      algorithms: [config.algorithm as jwt.Algorithm],
      issuer: config.issuer,
      audience: config.audience,
    };

    if (config.algorithm.startsWith('HS')) {
      // HMAC algorithm
      if (!config.secret) {
        throw new Error('Secret required for HMAC algorithm');
      }
      return jwt.verify(token, config.secret, verifyOptions);
    } else if (config.algorithm.startsWith('RS') || config.algorithm.startsWith('ES')) {
      // RSA or ECDSA algorithm
      if (config.jwksUrl) {
        // Use JWKS endpoint
        const key = await this.getKeyFromJWKS(token, config.jwksUrl);
        return jwt.verify(token, key, verifyOptions);
      } else if (config.publicKey) {
        // Use static public key
        return jwt.verify(token, config.publicKey, verifyOptions);
      } else {
        throw new Error('Public key or JWKS URL required for RSA/ECDSA algorithm');
      }
    } else {
      throw new Error(`Unsupported algorithm: ${config.algorithm}`);
    }
  }

  private async getKeyFromJWKS(token: string, jwksUrl: string): Promise<string> {
    let client = this.jwksClients.get(jwksUrl);
    
    if (!client) {
      client = jwksClient({
        jwksUri: jwksUrl,
        cache: true,
        cacheMaxAge: 600000, // 10 minutes
      });
      this.jwksClients.set(jwksUrl, client);
    }

    const decoded = jwt.decode(token, { complete: true });
    if (!decoded || !decoded.header.kid) {
      throw new Error('No kid found in token header');
    }

    const key = await client.getSigningKey(decoded.header.kid);
    return key.getPublicKey();
  }

  extractClaims(decodedToken: any, config: JWTConfig): any {
    const claims = this.getNestedProperty(decodedToken, config.claimsNamespace);
    
    if (!claims) {
      throw new Error(`Claims not found at namespace: ${config.claimsNamespace}`);
    }

    if (config.claimsFormat === 'stringified_json') {
      try {
        return JSON.parse(claims);
      } catch (e) {
        throw new Error('Failed to parse stringified JSON claims');
      }
    }

    return claims;
  }

  private getNestedProperty(obj: any, path: string): any {
    return path.split('.').reduce((current, key) => current?.[key], obj);
  }
}

// src/tenant-resolver.ts
export class TenantResolver {
  constructor(private db: Database) {}

  async resolveTenant(headers: Record<string, string>, token?: string): Promise<string> {
    // Strategy 1: Explicit tenant header
    if (headers['x-tenant-id']) {
      return headers['x-tenant-id'];
    }

    // Strategy 2: Extract from JWT
    if (token) {
      const decoded = jwt.decode(token) as any;
      if (decoded?.tenant_id) {
        return decoded.tenant_id;
      }
    }

    // Strategy 3: Extract from subdomain
    const host = headers['host'];
    if (host) {
      const subdomain = host.split('.')[0];
      const tenant = await this.db.getTenantBySlug(subdomain);
      if (tenant) {
        return tenant.id;
      }
    }

    throw new Error('Could not determine tenant');
  }
}

// src/webhook-handler.ts
import { Request, Response } from 'express';
import { JWTValidator } from './jwt-validator';
import { TenantResolver } from './tenant-resolver';
import { Database } from './database';
import { HasuraWebhookRequest, HasuraWebhookResponse } from './types';

export class WebhookHandler {
  constructor(
    private jwtValidator: JWTValidator,
    private tenantResolver: TenantResolver,
    private db: Database
  ) {}

  async validateRequest(req: Request, res: Response) {
    const startTime = Date.now();
    const headers = req.headers as Record<string, string>;
    const clientIp = req.ip;
    const userAgent = headers['user-agent'];

    try {
      // Extract JWT token
      const authHeader = headers['authorization'];
      if (!authHeader || !authHeader.startsWith('Bearer ')) {
        return res.status(401).json({ error: 'Missing or invalid Authorization header' });
      }

      const token = authHeader.substring(7);

      // Resolve tenant
      const tenantId = await this.tenantResolver.resolveTenant(headers, token);

      // Get active JWT configuration for tenant
      const jwtConfigs = await this.db.getActiveJWTConfigs(tenantId);
      if (jwtConfigs.length === 0) {
        throw new Error('No active JWT configuration found for tenant');
      }

      // Try to validate with each configuration
      let validatedClaims: any;
      let usedConfig: any;
      let lastError: Error | null = null;

      for (const config of jwtConfigs) {
        try {
          const decodedToken = await this.jwtValidator.validateToken(token, config);
          validatedClaims = this.jwtValidator.extractClaims(decodedToken, config);
          usedConfig = config;
          break;
        } catch (error) {
          lastError = error as Error;
          continue;
        }
      }

      if (!validatedClaims || !usedConfig) {
        throw lastError || new Error('JWT validation failed');
      }

      // Extract Hasura claims
      const hasuraRole = validatedClaims['x-hasura-role'];
      const hasuraUserId = validatedClaims['x-hasura-user-id'];

      if (!hasuraRole || !hasuraUserId) {
        throw new Error('Missing required Hasura claims');
      }

      // Apply role mappings if configured
      const mappedRole = await this.db.getMappedRole(usedConfig.id, hasuraRole);

      // Build response
      const response: HasuraWebhookResponse = {
        'x-hasura-role': mappedRole || hasuraRole,
        'x-hasura-user-id': String(hasuraUserId),
        'x-hasura-tenant-id': tenantId,
      };

      // Add any additional claims
      const allowedClaims = validatedClaims['x-hasura-allowed-roles'];
      if (allowedClaims) {
        response['x-hasura-allowed-roles'] = Array.isArray(allowedClaims) 
          ? allowedClaims.join(',') 
          : String(allowedClaims);
      }

      // Add custom session variables
      Object.keys(validatedClaims).forEach(key => {
        if (key.startsWith('x-hasura-') && !response[key]) {
          response[key] = String(validatedClaims[key]);
        }
      });

      // Log successful authentication
      await this.db.logAuthAttempt({
        tenantId,
        jwtConfigId: usedConfig.id,
        userId: hasuraUserId,
        success: true,
        ipAddress: clientIp,
        userAgent,
      });

      // Add performance metrics
      res.set('X-Auth-Time', `${Date.now() - startTime}ms`);
      
      return res.status(200).json(response);

    } catch (error) {
      const err = error as Error;
      
      // Log failed authentication
      await this.db.logAuthAttempt({
        tenantId: await this.tenantResolver.resolveTenant(headers, null).catch(() => null),
        success: false,
        errorMessage: err.message,
        ipAddress: clientIp,
        userAgent,
      });

      return res.status(401).json({ error: err.message });
    }
  }
}

// src/admin-api.ts
import { Router } from 'express';
import { Database } from './database';

export function createAdminAPI(db: Database): Router {
  const router = Router();

  // Create JWT configuration
  router.post('/tenants/:tenantId/jwt-configs', async (req, res) => {
    try {
      const { tenantId } = req.params;
      const config = req.body;

      // Validate configuration
      if (!config.algorithm || !config.name) {
        return res.status(400).json({ error: 'Missing required fields' });
      }

      // Validate algorithm-specific requirements
      if (config.algorithm.startsWith('HS') && !config.secret) {
        return res.status(400).json({ error: 'Secret required for HMAC algorithms' });
      }

      if ((config.algorithm.startsWith('RS') || config.algorithm.startsWith('ES')) 
          && !config.publicKey && !config.jwksUrl) {
        return res.status(400).json({ 
          error: 'Public key or JWKS URL required for RSA/ECDSA algorithms' 
        });
      }

      const created = await db.createJWTConfig(tenantId, config);
      res.status(201).json(created);
    } catch (error) {
      res.status(500).json({ error: (error as Error).message });
    }
  });

  // Update JWT configuration
  router.put('/jwt-configs/:configId', async (req, res) => {
    try {
      const { configId } = req.params;
      const updates = req.body;

      const updated = await db.updateJWTConfig(configId, updates);
      res.json(updated);
    } catch (error) {
      res.status(500).json({ error: (error as Error).message });
    }
  });

  // List JWT configurations for a tenant
  router.get('/tenants/:tenantId/jwt-configs', async (req, res) => {
    try {
      const { tenantId } = req.params;
      const configs = await db.getJWTConfigs(tenantId);
      
      // Don't expose secrets in the response
      const sanitized = configs.map(config => ({
        ...config,
        secret: config.secret ? '***' : undefined,
        publicKey: config.publicKey ? '***' : undefined,
      }));
      
      res.json(sanitized);
    } catch (error) {
      res.status(500).json({ error: (error as Error).message });
    }
  });

  // Delete JWT configuration
  router.delete('/jwt-configs/:configId', async (req, res) => {
    try {
      const { configId } = req.params;
      await db.deleteJWTConfig(configId);
      res.status(204).send();
    } catch (error) {
      res.status(500).json({ error: (error as Error).message });
    }
  });

  // Get authentication logs
  router.get('/tenants/:tenantId/auth-logs', async (req, res) => {
    try {
      const { tenantId } = req.params;
      const { limit = 100, offset = 0 } = req.query;
      
      const logs = await db.getAuthLogs(
        tenantId, 
        Number(limit), 
        Number(offset)
      );
      
      res.json(logs);
    } catch (error) {
      res.status(500).json({ error: (error as Error).message });
    }
  });

  return router;
}

// src/server.ts
import express from 'express';
import { WebhookHandler } from './webhook-handler';
import { JWTValidator } from './jwt-validator';
import { TenantResolver } from './tenant-resolver';
import { Database } from './database';
import { createAdminAPI } from './admin-api';

const app = express();
app.use(express.json());

// Initialize dependencies
const db = new Database();
const jwtValidator = new JWTValidator();
const tenantResolver = new TenantResolver(db);
const webhookHandler = new WebhookHandler(jwtValidator, tenantResolver, db);

// Webhook endpoint for Hasura
app.post('/validate', (req, res) => webhookHandler.validateRequest(req, res));

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'healthy' });
});

// Admin API (protect this with appropriate authentication)
app.use('/admin', authenticateAdmin, createAdminAPI(db));

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`Auth webhook service listening on port ${PORT}`);
});
```

### 4. Docker Compose Configuration

```yaml
version: '3.8'

services:
  hasura:
    build:
      context: .
      dockerfile: Dockerfile
    environment:
      METADATA_PATH: /app/metadata.json
      AUTHN_CONFIG_PATH: /app/auth_config.json
    ports:
      - "3000:3000"
    depends_on:
      - auth-webhook
      - postgres
    volumes:
      - ./metadata.json:/app/metadata.json
      - ./auth_config.json:/app/auth_config.json

  auth-webhook:
    build:
      context: ./auth-webhook
      dockerfile: Dockerfile
    environment:
      DATABASE_URL: postgresql://user:password@postgres:5432/auth_db
      PORT: 3000
    ports:
      - "3001:3000"
    depends_on:
      - postgres
      - redis

  postgres:
    image: postgres:16
    environment:
      POSTGRES_DB: auth_db
      POSTGRES_USER: user
      POSTGRES_PASSWORD: password
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./init.sql:/docker-entrypoint-initdb.d/init.sql

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

volumes:
  postgres_data:
```

### 5. Caching Strategy

Implement caching to improve performance:

```typescript
// src/cache.ts
import Redis from 'ioredis';

export class JWTConfigCache {
  private redis: Redis;
  private ttl: number = 300; // 5 minutes

  constructor(redisUrl: string) {
    this.redis = new Redis(redisUrl);
  }

  async get(tenantId: string): Promise<JWTConfig[] | null> {
    const cached = await this.redis.get(`jwt_configs:${tenantId}`);
    return cached ? JSON.parse(cached) : null;
  }

  async set(tenantId: string, configs: JWTConfig[]): Promise<void> {
    await this.redis.setex(
      `jwt_configs:${tenantId}`,
      this.ttl,
      JSON.stringify(configs)
    );
  }

  async invalidate(tenantId: string): Promise<void> {
    await this.redis.del(`jwt_configs:${tenantId}`);
  }
}
```

## Security Considerations

1. **Secure Storage**: Encrypt JWT secrets in the database
2. **Rate Limiting**: Implement rate limiting on the webhook endpoint
3. **Audit Logging**: Log all authentication attempts
4. **HTTPS**: Always use HTTPS in production
5. **Network Security**: Keep the webhook service in a private network
6. **Admin API Security**: Protect admin endpoints with strong authentication

## Performance Optimizations

1. **Caching**: Cache JWT configurations and JWKS keys
2. **Connection Pooling**: Use database connection pooling
3. **Async Processing**: Log audit events asynchronously
4. **Health Checks**: Implement proper health checks for all services
5. **Monitoring**: Add metrics for authentication performance

## Testing

### Unit Tests

```typescript
// tests/jwt-validator.test.ts
import { JWTValidator } from '../src/jwt-validator';
import jwt from 'jsonwebtoken';

describe('JWTValidator', () => {
  const validator = new JWTValidator();

  test('validates HMAC token', async () => {
    const secret = 'test-secret';
    const payload = {
      'https://hasura.io/jwt/claims': {
        'x-hasura-role': 'user',
        'x-hasura-user-id': '123',
      },
    };
    
    const token = jwt.sign(payload, secret, { algorithm: 'HS256' });
    const config = {
      id: '1',
      tenantId: 'tenant1',
      algorithm: 'HS256',
      secret,
      claimsNamespace: 'https://hasura.io/jwt/claims',
      claimsFormat: 'json' as const,
    };

    const decoded = await validator.validateToken(token, config);
    expect(decoded).toMatchObject(payload);
  });
});
```

### Integration Tests

```typescript
// tests/webhook.test.ts
import request from 'supertest';
import { app } from '../src/server';

describe('Webhook Endpoint', () => {
  test('validates valid JWT', async () => {
    const token = 'valid.jwt.token';
    
    const response = await request(app)
      .post('/validate')
      .set('Authorization', `Bearer ${token}`)
      .set('X-Tenant-ID', 'tenant1');

    expect(response.status).toBe(200);
    expect(response.body).toHaveProperty('x-hasura-role');
    expect(response.body).toHaveProperty('x-hasura-user-id');
    expect(response.body).toHaveProperty('x-hasura-tenant-id');
  });

  test('rejects invalid JWT', async () => {
    const response = await request(app)
      .post('/validate')
      .set('Authorization', 'Bearer invalid.token');

    expect(response.status).toBe(401);
  });
});
```

## Deployment Considerations

1. **High Availability**: Deploy multiple instances of the webhook service
2. **Load Balancing**: Use a load balancer for the webhook service
3. **Database Replication**: Use read replicas for better performance
4. **Backup Strategy**: Regular backups of JWT configurations
5. **Disaster Recovery**: Plan for service failures and data recovery

## Monitoring and Observability

1. **Metrics to Track**:
   - Authentication success/failure rates
   - Response times
   - Cache hit rates
   - Database query performance

2. **Alerts**:
   - High failure rates
   - Slow response times
   - Database connection issues
   - Certificate expiration (for JWKS)

3. **Dashboards**:
   - Real-time authentication metrics
   - Per-tenant usage statistics
   - Error analysis

## Migration Guide

To migrate from single JWT to multi-tenant:

1. Deploy the webhook service
2. Import existing JWT configuration to the database
3. Update Hasura auth config to use webhook mode
4. Test with existing tokens
5. Gradually onboard new tenants

## Troubleshooting

Common issues and solutions:

1. **Token validation fails**:
   - Check JWT configuration in database
   - Verify algorithm matches
   - Check token expiration

2. **Performance issues**:
   - Enable caching
   - Check database indexes
   - Monitor webhook service resources

3. **Tenant resolution fails**:
   - Verify tenant identification strategy
   - Check headers being forwarded

## Conclusion

This webhook-based approach provides a flexible, scalable solution for multi-tenant JWT authentication in Hasura v3. It allows dynamic configuration management while maintaining security and performance.