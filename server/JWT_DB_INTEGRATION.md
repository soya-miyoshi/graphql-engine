# JWT Database Integration

This implementation allows Hasura to fetch JWT configurations from a database table instead of using environment variables.

## Overview

The new implementation:
1. Extracts the issuer from the JWT payload without verifying the signature
2. Queries the `jwt_config` table using the issuer
3. Uses the retrieved configuration to verify the JWT

## Setup

### 1. Create the JWT Config Table

Run the SQL schema in `jwt_config_schema.sql` to create the necessary table.

### 2. Insert JWT Configurations

For each JWT issuer you want to support, insert a configuration:

```sql
INSERT INTO jwt_config (issuer, config) VALUES (
    'https://auth0.com/my-tenant',
    '{
        "type": "RS256",
        "jwk_url": "https://auth0.com/my-tenant/.well-known/jwks.json",
        "audience": "my-api",
        "claims_namespace": "https://hasura.io/jwt/claims"
    }'::jsonb
);
```

### 3. Enable Database JWT Mode

To use the database-backed JWT configuration, you'll need to:

1. Set up the admin secret as usual
2. Enable the JWT database mode (implementation depends on how you want to configure this - could be an environment variable like `HASURA_GRAPHQL_JWT_DB_ENABLED=true`)

## Configuration Format

The `config` JSONB column supports all the same options as the traditional `HASURA_GRAPHQL_JWT_SECRET`:

- `type`: Algorithm type (RS256, HS256, etc.)
- `key`: The key (for symmetric algorithms) or public key (for asymmetric)
- `jwk_url`: URL to fetch JWK Set
- `audience`: Expected audience
- `claims_namespace`: Namespace for Hasura claims
- `claims_namespace_path`: JSON path to claims
- `claims_format`: Format of claims (json or stringified_json)
- `claims_map`: Custom claims mapping
- `allowed_skew`: Allowed time skew in seconds
- `header`: Header configuration

## Integration Points

The main integration happens in the authentication flow:

1. **Hasura.Server.Auth.JWT.Simple** - New module with database-aware JWT processing
2. **Hasura.Server.Auth** - Modified to support the new auth mode `AMAdminSecretAndJWTDB`

## Testing

To test the implementation:

1. Insert a JWT configuration for your issuer
2. Generate a JWT with that issuer
3. Make a request to Hasura with the JWT
4. Verify that Hasura correctly validates the JWT using the database configuration

## Migration from Environment Variables

To migrate existing JWT configurations:

1. Extract your current JWT configuration from environment variables
2. Convert it to the JSON format
3. Insert it into the `jwt_config` table with the appropriate issuer
4. Switch to using the database mode

Example migration:
```sql
-- If you had HASURA_GRAPHQL_JWT_SECRET='{"type":"RS256","jwk_url":"https://..."}'
INSERT INTO jwt_config (issuer, config) VALUES (
    'your-issuer-url',
    '{"type":"RS256","jwk_url":"https://..."}'::jsonb
);
```