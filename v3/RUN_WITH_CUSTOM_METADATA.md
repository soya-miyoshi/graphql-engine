# Running Hasura v3 Engine with Custom Metadata

This guide shows you how to run the Hasura v3 engine with your own metadata to create a custom GraphQL API.

## Prerequisites

1. **Rust toolchain** installed
2. **Data connector** running (e.g., custom-connector, postgres-connector)
3. **Metadata file** prepared (see `METADATA_GUIDE.md`)

## Quick Start

### 1. Using the Custom Connector (No Database Required)

The easiest way to start is with the custom connector that doesn't require a database:

```bash
# Terminal 1: Run the custom connector
cargo run --bin custom-connector

# Terminal 2: Run the engine with example metadata
cargo run --bin engine -- \
    --metadata-path example-metadata.json \
    --authn-config-path static/auth/noauth_config_v3.json \
    --port 3000
```

### 2. Using Docker Compose (With Postgres)

```bash
# Start all services
docker compose up -d

# Run engine with your metadata
cargo run --bin engine -- \
    --metadata-path your-metadata.json \
    --authn-config-path static/auth/auth_config_v3.json \
    --otlp-endpoint http://localhost:4317 \
    --port 3000
```

## Step-by-Step Guide

### Step 1: Prepare Your Data Connector

Choose one of these options:

**Option A: Custom Connector (For Testing)**
```bash
cargo run --bin custom-connector
# Runs on http://localhost:8102
```

**Option B: Postgres Connector**
```bash
docker compose up -d postgres postgres_connector
# Runs on http://localhost:8082
```

### Step 2: Get Connector Schema

Fetch the schema from your connector:

```bash
# For custom connector
curl http://localhost:8102/schema > connector-schema.json

# For postgres connector  
curl http://localhost:8082/schema > connector-schema.json
```

### Step 3: Create Your Metadata

1. Copy `example-metadata.json` as a starting point
2. Update the connector URL in `DataConnectorLink`
3. Replace the schema section with your connector's schema
4. Define your object types, models, and relationships

### Step 4: Choose Authentication Mode

**For Development (No Auth):**
```bash
--authn-config-path static/auth/noauth_config_v3.json
```

**For Webhook Auth:**
```bash
# First run the auth webhook
cargo run --bin dev-auth-webhook

# Then use webhook auth config
--authn-config-path static/auth/auth_config_v3.json
```

### Step 5: Run the Engine

```bash
cargo run --bin engine -- \
    --metadata-path <your-metadata.json> \
    --authn-config-path <auth-config.json> \
    --port 3000 \
    --otlp-endpoint http://localhost:4317  # Optional: for tracing
```

```bash
cargo run --bin engine -- \
    --metadata-path ./example-metadata.json \
    --authn-config-path ./static/auth/auth_config_v3.json \
    --port 3000 \
    --otlp-endpoint http://localhost:4317  # Optional: for tracing
```

### Step 6: Test Your GraphQL API

1. Open http://localhost:3000 in your browser (GraphiQL interface)
2. Add headers if using auth: `{"x-hasura-role": "admin"}`
3. Try a query:

```graphql
query {
  users {
    id
    name
    email
    posts {
      id
      title
    }
  }
}
```

## Command Line Options

```bash
cargo run --bin engine -- --help
```

Key options:
- `--metadata-path`: Path to your metadata JSON file
- `--authn-config-path`: Authentication configuration
- `--port`: Server port (default: 3000)
- `--host`: Host IP (default: all interfaces)
- `--otlp-endpoint`: OpenTelemetry endpoint for tracing
- `--enable-cors`: Enable CORS support
- `--expose-internal-errors`: Show detailed errors (dev only)

## Using Just Commands

The repository includes helpful `just` commands:

```bash
# Run with test metadata and dependencies
just run

# Run with SQL interface enabled
just run-for-sql

# Run with custom metadata
just run path/to/metadata.json

# Start only the dependencies
just start-docker-test-deps
```

## Environment Variables

You can also use environment variables:

```bash
export METADATA_PATH=my-metadata.json
export AUTHN_CONFIG_PATH=static/auth/noauth_config_v3.json
export OTLP_ENDPOINT=http://localhost:4317
export PORT=3000

cargo run --bin engine
```

## Validating Your Metadata

The engine validates metadata on startup. Check for:
- Valid JSON syntax
- Correct object references
- Matching connector schemas
- Valid GraphQL names

Common validation errors:
- "Unknown object type" - Check type definitions
- "Unknown model" - Ensure model is defined
- "Invalid field mapping" - Verify connector schema

## Development Workflow

1. **Start simple**: Begin with one model and no relationships
2. **Test incrementally**: Add features one at a time
3. **Check logs**: Engine provides detailed error messages
4. **Use GraphiQL**: Test queries interactively
5. **Enable tracing**: Use Jaeger for debugging (http://localhost:4002)

## Next Steps

- Add more models and relationships
- Configure permissions for different roles
- Add custom commands (mutations)
- Set up filtering and ordering
- Configure aggregations

See `METADATA_GUIDE.md` for detailed metadata configuration options.
