# Hasura v3 Metadata Guide

This guide explains how to create and configure metadata for the Hasura GraphQL Engine v3 to define your GraphQL API.

## Overview

In Hasura v3, the GraphQL schema is completely determined by OpenDDS (Open Data Domain Specification) metadata. This metadata defines:
- Data sources (connectors)
- Object types and their fields
- Models (for queries)
- Commands (for mutations/queries)
- Relationships between types
- Permissions per role
- GraphQL configuration

## Basic Metadata Structure

The metadata follows this structure:

```json
{
  "version": "v3",
  "subgraphs": [
    {
      "name": "default",
      "objects": [
        // Your metadata objects go here
      ]
    }
  ]
}
```

## Step-by-Step Guide

### 1. Define Data Connector

First, configure your data source:

```json
{
  "kind": "DataConnectorLink",
  "version": "v1",
  "definition": {
    "name": "my_db",
    "url": {
      "singleUrl": {
        "value": "http://localhost:8082"  // Your connector URL
      }
    },
    "headers": {},  // Optional auth headers
    "schema": {
      // NDC schema - usually fetched from connector
    }
  }
}
```

### 2. Define Object Types

Define the GraphQL types that map to your data:

```json
{
  "kind": "ObjectType",
  "version": "v1",
  "definition": {
    "name": "User",
    "fields": [
      {
        "name": "id",
        "type": "Int!"
      },
      {
        "name": "name",
        "type": "String!"
      },
      {
        "name": "email",
        "type": "String!"
      }
    ],
    "dataConnectorTypeMapping": [
      {
        "dataConnectorName": "my_db",
        "dataConnectorObjectType": "users",
        "fieldMapping": {
          "id": {
            "column": {
              "name": "id"
            }
          },
          "name": {
            "column": {
              "name": "name"
            }
          },
          "email": {
            "column": {
              "name": "email"
            }
          }
        }
      }
    ]
  }
}
```

### 3. Define Models (for Queries)

Models define how to query your data:

```json
{
  "kind": "Model",
  "version": "v1",
  "definition": {
    "name": "Users",
    "objectType": "User",
    "source": {
      "dataConnectorName": "my_db",
      "collection": "users"
    },
    "orderableFields": [
      {
        "fieldName": "id",
        "orderByDirections": {
          "enableAll": true
        }
      },
      {
        "fieldName": "name",
        "orderByDirections": {
          "enableAll": true
        }
      }
    ],
    "graphql": {
      "selectOne": {
        "queryRootField": "user"
      },
      "selectMany": {
        "queryRootField": "users"
      }
    }
  }
}
```

### 4. Define Commands (for Mutations/Custom Queries)

Commands map to functions or procedures:

```json
{
  "kind": "Command",
  "version": "v1",
  "definition": {
    "name": "CreateUser",
    "arguments": [
      {
        "name": "name",
        "type": "String!"
      },
      {
        "name": "email",
        "type": "String!"
      }
    ],
    "outputType": "User",
    "source": {
      "dataConnectorName": "my_db",
      "dataConnectorCommand": {
        "procedure": "create_user"
      }
    },
    "graphql": {
      "rootFieldName": "createUser",
      "rootFieldKind": "Mutation"
    }
  }
}
```

### 5. Define Relationships

Connect related types:

```json
{
  "kind": "Relationship",
  "version": "v1",
  "definition": {
    "name": "posts",
    "sourceType": "User",
    "target": {
      "model": {
        "name": "Posts",
        "relationshipType": "Array"
      }
    },
    "mapping": [
      {
        "source": {
          "fieldPath": [
            {
              "fieldName": "id"
            }
          ]
        },
        "target": {
          "modelField": [
            {
              "fieldName": "authorId"
            }
          ]
        }
      }
    ]
  }
}
```

### 6. Define Permissions

Control access per role:

```json
{
  "kind": "ModelPermissions",
  "version": "v1",
  "definition": {
    "modelName": "Users",
    "permissions": [
      {
        "role": "admin",
        "select": {
          "filter": null  // No filter - can see all
        }
      },
      {
        "role": "user",
        "select": {
          "filter": {
            "fieldComparison": {
              "field": "id",
              "operator": "_eq",
              "value": {
                "sessionVariable": "x-hasura-user-id"
              }
            }
          }
        }
      }
    ]
  }
}
```

### 7. Configure GraphQL

Set GraphQL-specific options:

```json
{
  "kind": "GraphqlConfig",
  "version": "v1",
  "definition": {
    "query": {
      "rootOperationTypeName": "Query",
      "argumentsInput": {
        "fieldName": "args"
      },
      "limitInput": {
        "fieldName": "limit"
      },
      "offsetInput": {
        "fieldName": "offset"
      },
      "filterInput": {
        "fieldName": "where",
        "operatorNames": {
          "and": "_and",
          "or": "_or",
          "not": "_not",
          "isNull": "_is_null"
        }
      },
      "orderByInput": {
        "fieldName": "order_by",
        "enumDirectionValues": {
          "asc": "Asc",
          "desc": "Desc"
        }
      }
    },
    "mutation": {
      "rootOperationTypeName": "Mutation"
    }
  }
}
```

## Complete Example

Here's a minimal complete metadata file:

```json
{
  "version": "v3",
  "subgraphs": [
    {
      "name": "default",
      "objects": [
        {
          "kind": "DataConnectorLink",
          "version": "v1",
          "definition": {
            "name": "my_db",
            "url": {
              "singleUrl": {
                "value": "http://localhost:8082"
              }
            },
            "schema": {
              // Your NDC schema here
            }
          }
        },
        {
          "kind": "ObjectType",
          "version": "v1",
          "definition": {
            "name": "User",
            "fields": [
              {
                "name": "id",
                "type": "Int!"
              },
              {
                "name": "name",
                "type": "String!"
              }
            ],
            "dataConnectorTypeMapping": [
              {
                "dataConnectorName": "my_db",
                "dataConnectorObjectType": "users",
                "fieldMapping": {
                  "id": { "column": { "name": "id" } },
                  "name": { "column": { "name": "name" } }
                }
              }
            ]
          }
        },
        {
          "kind": "Model",
          "version": "v1",
          "definition": {
            "name": "Users",
            "objectType": "User",
            "source": {
              "dataConnectorName": "my_db",
              "collection": "users"
            },
            "graphql": {
              "selectMany": {
                "queryRootField": "users"
              }
            }
          }
        }
      ]
    }
  ]
}
```

## Running with Your Metadata

1. Save your metadata to a file (e.g., `my_metadata.json`)

2. Run the engine:
```bash
cargo run --bin engine -- \
    --metadata-path my_metadata.json \
    --authn-config-path static/auth/noauth_config_v3.json \
    --port 3000
```

3. Access GraphiQL at http://localhost:3000

4. Test your queries:
```graphql
query {
  users {
    id
    name
  }
}
```

## Common Issues and Troubleshooting

### 1. "Unknown object type" Error
**Problem**: Reference to an undefined type
**Solution**: Ensure the ObjectType is defined before being referenced in Models

### 2. "Field not found in connector schema"
**Problem**: Field mapping doesn't match connector
**Solution**: Check connector schema with `curl http://localhost:8102/schema`

### 3. "Invalid GraphQL name"
**Problem**: Using reserved words or invalid characters
**Solution**: Use valid GraphQL identifiers (start with letter, alphanumeric + underscore)

### 4. No Data Returned
**Possible Causes**:
- Connector not running or wrong URL
- Permission filters blocking access
- Empty collections in connector

### 5. Relationship Not Working
**Check**:
- Source and target models exist
- Field types match (Int to Int, String to String)
- Mapping fields exist in both types

### 6. Command/Mutation Fails
**Verify**:
- Procedure exists in connector
- Argument types match exactly
- CommandPermissions allow execution for your role

## Debugging Tips

1. **Enable detailed errors**: Add `--expose-internal-errors` flag
2. **Check connector health**: `curl http://localhost:8102/health`
3. **Validate metadata separately**: The engine validates on startup
4. **Use tracing**: Connect Jaeger for detailed request flow
5. **Start minimal**: One model, no relationships, then build up

## Best Practices

- Version control your metadata files
- Use meaningful names for models and relationships
- Document your permission logic
- Keep related objects in the same subgraph
- Test with different roles to verify permissions
- Use consistent naming conventions (camelCase for GraphQL, snake_case for DB)