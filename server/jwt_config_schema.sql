-- JWT Configuration Table Schema
-- This table stores JWT configurations indexed by issuer

CREATE TABLE IF NOT EXISTS jwt_config (
    issuer TEXT PRIMARY KEY,
    config JSONB NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Create an index on issuer for fast lookups
CREATE INDEX IF NOT EXISTS idx_jwt_config_issuer ON jwt_config(issuer);

-- Example insert for a JWT configuration
-- The config column should contain a JSON object with the following structure:
/*
INSERT INTO jwt_config (issuer, config) VALUES (
    'https://your-auth-provider.com',
    '{
        "type": "RS256",
        "jwk_url": "https://your-auth-provider.com/.well-known/jwks.json",
        "audience": "your-audience",
        "claims_namespace": "https://hasura.io/jwt/claims",
        "claims_format": "json"
    }'::jsonb
);
*/

-- Alternative example with inline JWK key:
/*
INSERT INTO jwt_config (issuer, config) VALUES (
    'my-custom-issuer',
    '{
        "type": "HS256",
        "key": "your-secret-key-here",
        "claims_map": {
            "x-hasura-allowed-roles": {
                "path": "$.roles"
            },
            "x-hasura-default-role": {
                "path": "$.role",
                "default": "user"
            },
            "x-hasura-user-id": {
                "path": "$.sub"
            }
        }
    }'::jsonb
);
*/