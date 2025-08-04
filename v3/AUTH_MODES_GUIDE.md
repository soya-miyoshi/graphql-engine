# Authentication Modes Guide for Hasura GraphQL Engine v3

This guide explains the authentication system in Hasura GraphQL Engine v3 and how to add new authentication modes.

## Overview

The authentication system in v3 is modular and supports multiple authentication modes. The main authentication logic is located in `/crates/auth/hasura-authn/src/lib.rs`.

## Supported Authentication Modes

Currently, the engine supports **3 authentication modes**:

1. **Webhook** - External webhook-based authentication
   - Sends requests to an external service for authentication
   - Supports custom headers and request configuration
   - Location: `/crates/auth/hasura-authn-webhook/`

2. **JWT** (JSON Web Token) - Token-based authentication
   - Validates JWT tokens from requests
   - Supports various signing algorithms
   - Location: `/crates/auth/hasura-authn-jwt/`

3. **NoAuth** - No authentication (development mode)
   - Bypasses authentication for development
   - Should not be used in production
   - Location: `/crates/auth/hasura-authn-noauth/`

## Authentication Configuration Structure

The authentication configuration has evolved through multiple versions:

- **v1**: Basic auth with role emulation support (deprecated)
- **v2**: Simplified configuration without role emulation (deprecated)
- **v3**: Current stable version
- **v4**: Latest version with alternative auth modes support

### Example Auth Config (v3)

```json
{
  "version": "v3",
  "definition": {
    "mode": {
      "webhook": {
        "method": "POST",
        "url": {
          "value": "http://localhost:3050/validate-request"
        }
      }
    }
  }
}
```

### Example Auth Config (v4) with Alternative Modes

```json
{
  "version": "v4",
  "definition": {
    "mode": {
      "webhook": {
        "method": "GET",
        "url": {
          "value": "http://auth_hook:3050/validate-request"
        }
      }
    },
    "alternativeModes": [
      {
        "identifier": "internal",
        "config": {
          "jwt": {
            "claimsConfig": {
              "namespace": {
                "claimsFormat": "Json",
                "location": "/claims.jwt.hasura.io"
              }
            },
            "tokenLocation": {
              "type": "BearerAuthorization"
            },
            "key": {
              "fixed": {
                "algorithm": "HS256",
                "key": {
                  "valueFromEnv": "AUTH_SECRET"
                }
              }
            }
          }
        }
      }
    ]
  }
}
```

## How to Add a New Authentication Mode

To add a new authentication mode (e.g., API Key authentication), follow these steps:

### 1. Create a New Auth Crate

Create a new crate under `/crates/auth/`:

```bash
mkdir -p crates/auth/hasura-authn-apikey/src
```

Create `Cargo.toml`:

```toml
[package]
name = "hasura-authn-apikey"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
axum = { workspace = true }
hasura-authn-core = { path = "../hasura-authn-core" }
schemars = { workspace = true, features = ["preserve_order"] }
serde = { workspace = true }
serde_json = { workspace = true }
```

### 2. Define Configuration Structure

In `crates/auth/hasura-authn-apikey/src/lib.rs`:

```rust
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ApiKeyConfig {
    /// Header name containing the API key
    pub header_name: String,
    /// Optional prefix (e.g., "Bearer")
    pub prefix: Option<String>,
}
```

### 3. Update Auth Mode Enums

In `/crates/auth/hasura-authn/src/lib.rs`, add your new mode to both enum definitions:

```rust
// Line ~17 - For v1/v2 compatibility
#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "AuthModeConfig")]
pub enum AuthModeConfig {
    Webhook(webhook::AuthHookConfig),
    Jwt(Box<jwt::JWTConfig>),
    NoAuth(noauth::NoAuthConfig),
    ApiKey(apikey::ApiKeyConfig), // New mode
}

// Line ~167 - For v3/v4
#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "AuthModeConfigV3")]
pub enum AuthModeConfigV3 {
    Webhook(webhook::AuthHookConfigV3),
    Jwt(Box<jwt::JWTConfig>),
    NoAuth(noauth::NoAuthConfig),
    ApiKey(apikey::ApiKeyConfig), // New mode
}
```

### 4. Add Import

Add the import at the top of `/crates/auth/hasura-authn/src/lib.rs`:

```rust
use hasura_authn_apikey as apikey;
```

### 5. Implement Authentication Logic

Update the `authenticate` function (around line 502) to handle your new mode:

```rust
match &auth_mode {
    // ... existing cases ...
    
    PossibleAuthModeConfig::V1V2(AuthModeConfig::ApiKey(api_key_config))
    | PossibleAuthModeConfig::V3V4(AuthModeConfigV3::ApiKey(api_key_config)) => {
        apikey::authenticate_request(
            headers_map,
            api_key_config,
        )
        .map_err(AuthError::from)
    }
}
```

### 6. Add Validation (Optional)

If your auth mode requires specific validation, update the `validate_auth_config` function:

```rust
fn validate_auth_config(auth_config: &AuthConfig) -> Result<Vec<Warning>, Error> {
    // ... existing validation ...
    
    // Add API key specific validation
    match auth_config {
        AuthConfig::V3(AuthConfigV3 { mode }) | AuthConfig::V4(AuthConfigV4 { mode, .. }) => {
            if let AuthModeConfigV3::ApiKey(config) = &mode {
                // Validate header name format
                if config.header_name.is_empty() {
                    return Err(Error::InvalidApiKeyConfig("Header name cannot be empty".to_string()));
                }
            }
        }
        _ => {}
    }
}
```

### 7. Update Dependencies

Add your new crate to the main `hasura-authn` dependencies in its `Cargo.toml`:

```toml
[dependencies]
hasura-authn-apikey = { path = "../hasura-authn-apikey" }
```

## Usage Example

Once implemented, users can configure the new auth mode:

```json
{
  "version": "v3",
  "definition": {
    "mode": {
      "apiKey": {
        "headerName": "X-API-Key",
        "prefix": "Bearer"
      }
    }
  }
}
```

## Testing Your Auth Mode

1. Create unit tests in your auth crate
2. Add integration tests in `/crates/engine/tests/`
3. Test with the engine:

```bash
cargo run --bin engine -- \
    --metadata-path metadata.json \
    --authn-config-path auth_config_with_apikey.json \
    --port 3000
```

## Best Practices

1. **Error Handling**: Use proper error types that implement `tracing_util::TraceableError`
2. **Configuration**: Make configuration options clear and well-documented
3. **Security**: Never log sensitive information like tokens or keys
4. **Compatibility**: Ensure your mode works with both v3 and v4 auth configs
5. **Documentation**: Update this guide with your new auth mode details

## File Structure Reference

```
crates/auth/
├── hasura-authn/           # Main auth orchestration
├── hasura-authn-core/      # Core types (Identity, Role)
├── hasura-authn-jwt/       # JWT implementation
├── hasura-authn-noauth/    # NoAuth implementation
├── hasura-authn-webhook/   # Webhook implementation
└── hasura-authn-apikey/    # Your new auth mode
```

## Related Files

- Auth configuration parsing: `/crates/auth/hasura-authn/src/lib.rs`
- Auth middleware: `/crates/engine/src/middleware.rs`
- Example configs: `/static/auth/`