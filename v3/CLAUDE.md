# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Common Development Commands

### Build and Run

- **Build the project**: `cargo build --release --all-targets`
- **Run the engine**:
  `cargo run --bin engine -- --metadata-path <path> --authn-config-path <path>`
- **Run with Docker**: `docker compose up` (serves GraphiQL at
  http://localhost:3000)
- **Run for SQL testing**: `just run-for-sql`
- **Run with test metadata**: `just run`

### Engine CLI Options

The engine binary accepts the following command-line options and environment
variables:

#### Required Options

- `--metadata-path <PATH>` or `METADATA_PATH` - Path to OpenDD metadata file
  (e.g., metadata.json, open_dd.json)
- `--authn-config-path <PATH>` or `AUTHN_CONFIG_PATH` - Path to authentication
  configuration file

#### Server Configuration

- `--host <HOST>` or `HOST` - Host IP to listen on (default: all IPv4/IPv6
  addresses)
- `--port <PORT>` or `PORT` - Port to listen on (default: 3000)

#### Observability

- `--otlp-endpoint <URL>` or `OTLP_ENDPOINT` - OpenTelemetry collector endpoint
  for tracing
- `--export-traces-stdout` - Export traces to stdout
- `--log <LEVEL>` or `LOG_LEVEL` - Log level: trace, debug, info, warn, error
  (default: info)
- `--log-style <STYLE>` or `LOG_STYLE` - Log output style: plain, json (default:
  plain)

#### CORS Configuration

- `--enable-cors` or `ENABLE_CORS` - Enable CORS support
- `--cors-allow-origin <ORIGIN_LIST>` or `CORS_ALLOW_ORIGIN` - Comma-separated
  list of allowed origins

#### Development Options

- `--introspection-metadata <PATH>` or `INTROSPECTION_METADATA_FILE` - Metadata
  file served at `/metadata`
- `--expose-internal-errors` or `EXPOSE_INTERNAL_ERRORS` - Show internal errors
  (dev only)
- `--enable-sql-interface` or `ENABLE_SQL_INTERFACE` - Enable SQL query
  interface
- `--unstable-feature <FEATURES>` or `UNSTABLE_FEATURES` - Enable unstable
  features

#### Example Commands

```bash
# Basic usage
cargo run --bin engine -- \
    --metadata-path ./metadata.json \
    --authn-config-path ./auth_config.json

# With tracing and development features
cargo run --bin engine -- \
    --metadata-path ./open_dd.json \
    --authn-config-path ./auth_config.json \
    --otlp-endpoint http://localhost:4317 \
    --expose-internal-errors \
    --enable-cors \
    --port 3001

# Using environment variables
export METADATA_PATH=./my-project/metadata.json
export AUTHN_CONFIG_PATH=./my-project/auth_config.json
export OTLP_ENDPOINT=http://localhost:4317
export PORT=3000
cargo run --bin engine
```

### Testing

- **Run all tests**: `just test` (requires `cargo install cargo-nextest`)
- **Run specific test**: `cargo nextest run <test_name>`
- **Run doctests**: `cargo test --doc`
- **Watch mode**: `just watch` (continuously runs tests)
- **Update golden files**: `just update-golden-files` or
  `UPDATE_GOLDENFILES=1 just test`
- **Review insta snapshots**: `cargo insta review`

### Code Quality

- **Lint**: `cargo clippy --all-targets --no-deps`
- **Format check**: `cargo fmt --check`
- **Fix formatting**: `just fix-format`
- **Fix clippy issues**:
  `cargo clippy --all-targets --no-deps --fix --allow-no-vcs`
- **Check unused dependencies**: `cargo machete --with-metadata`

### Docker Dependencies

- **Start test dependencies**: `just start-docker-test-deps`
- **Stop Docker**: `just stop-docker`
- **Refresh Docker images**: `just docker-refresh`

### Benchmarks

- **Run lexer benchmarks**: `cargo bench -p lang-graphql "lexer"`
- **Run parser benchmarks**: `cargo bench -p lang-graphql "parser"`
- **Run validation benchmarks**: `cargo bench -p lang-graphql "validation/.*"`

## High-Level Architecture

### Core Components

1. **open-dds** (`crates/open-dds/`)
   - Contains Open Data Domain Specification metadata structures
   - Defines data models, permissions, connectors configuration

2. **metadata-resolve** (`crates/metadata-resolve/`)
   - Validates and resolves Open DDS metadata
   - Creates intermediate structures for schema generation
   - Handles multi-stage metadata resolution pipeline

3. **GraphQL Layer** (`crates/graphql/`)
   - **lang-graphql**: Complete GraphQL spec implementation (lexer, parser,
     validation, introspection)
   - **schema**: Generates GraphQL schema from resolved metadata
   - **ir**: Intermediate representation for GraphQL operations
   - **frontend**: Entry points for GraphQL request processing

4. **execute** (`crates/execute/`)
   - Core query execution engine
   - Handles NDC (Native Data Connector) communication
   - Manages remote joins and request planning

5. **engine** (`crates/engine/`)
   - Main server binary entry point
   - HTTP server setup and routing
   - Authentication and middleware

6. **Other Frontends**
   - **jsonapi**: JSON:API frontend implementation
   - **sql**: SQL frontend (in development)

### Key Design Principles

1. **Separation of Concerns**: Clear boundary between Open DDS metadata, v3
   engine, and NDC data connectors
2. **Reliable Startup**: Schema is fully determined by Open DDS metadata,
   independent of data connector availability
3. **Configuration over Convention**: Explicit metadata configuration without
   implicit conventions

### Data Flow

1. User provides Open DDS metadata → Metadata resolution → GraphQL schema
   generation
2. GraphQL request → Parse → Validate → Generate IR → Plan execution → Execute
   via NDC → Return response

### Testing Infrastructure

- Integration tests use Docker Compose for data connectors
- Golden file tests for snapshot testing
- Tests require authentication with `{"x-hasura-role": "admin"}` header
- Test metadata files in `static/` and various test directories
