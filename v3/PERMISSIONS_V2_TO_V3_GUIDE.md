# Permissions Guide: Hasura v2 to v3 Migration

This guide explains the differences in permission systems between Hasura v2 and v3, and how to migrate complex permissions that involve database queries through relationships.

## Overview

Hasura v3 supports complex permissions similar to v2, including relationship-based permissions that perform database queries. However, the metadata structure and syntax have changed significantly.

## Key Differences

### 1. Metadata Format

**v2 Format (YAML)**:
```yaml
table:
  schema: public
  name: articles
insert_permissions:
  - role: user
    permission:
      check: { ... }
```

**v3 Format (JSON with OpenDD)**:
```json
{
  "kind": "ModelPermissions",
  "version": "v1",
  "definition": {
    "modelName": "Articles",
    "permissions": [ ... ]
  }
}
```

### 2. Permission Structure

v3 uses a more structured approach with explicit predicate types:
- `fieldComparison` - Compare a field with a value
- `fieldIsNull` - Check if a field is null
- `relationship` - Traverse relationships and apply predicates
- `and`, `or`, `not` - Boolean logic operators
- `nestedField` - Access nested fields in objects

### 3. Supported Permission Types

**Currently Implemented in v3**:
- ✅ Select permissions with complex filters
- ✅ Argument presets for models and commands
- ✅ Type-level field permissions

**Planned/Partial Support**:
- ⚠️ Insert permissions (defined but not fully implemented)
- ⚠️ Update permissions (defined but not fully implemented)
- ⚠️ Delete permissions (defined but not fully implemented)

## Relationship-Based Permissions

Yes, v3 **fully supports** permissions that involve database queries through relationships!

### Simple Example

**v2 Permission**:
```yaml
select_permissions:
  - role: user
    permission:
      filter:
        author:
          id:
            _eq: X-Hasura-User-Id
```

**v3 Equivalent**:
```json
{
  "role": "user",
  "select": {
    "filter": {
      "relationship": {
        "name": "author",
        "predicate": {
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
  }
}
```

### Complex Example: Multi-Level Relationships with OR Logic

Let's translate your complex v2 permission example:

**v2 Permission**:
```yaml
insert_permissions:
  - role: user
    permission:
      check:
        space:
          _or:
            - user:
                keycloak:
                  keycloak_id:
                    _eq: X-Hasura-User-Id
            - policies:
                _and:
                  - policy_write_this_space: {}
                  - role_policies:
                      role:
                        user_roles:
                          user:
                            keycloak:
                              keycloak_id:
                                _eq: X-Hasura-User-Id
```

**v3 Equivalent**:
```json
{
  "kind": "ModelPermissions",
  "version": "v1",
  "definition": {
    "modelName": "YourModelName",
    "permissions": [
      {
        "role": "user",
        "select": {
          "filter": {
            "relationship": {
              "name": "space",
              "predicate": {
                "or": [
                  {
                    "relationship": {
                      "name": "user",
                      "predicate": {
                        "relationship": {
                          "name": "keycloak",
                          "predicate": {
                            "fieldComparison": {
                              "field": "keycloak_id",
                              "operator": "_eq",
                              "value": {
                                "sessionVariable": "x-hasura-user-id"
                              }
                            }
                          }
                        }
                      }
                    }
                  },
                  {
                    "relationship": {
                      "name": "policies",
                      "predicate": {
                        "and": [
                          {
                            "fieldComparison": {
                              "field": "policy_write_this_space",
                              "operator": "_eq",
                              "value": {
                                "literal": true
                              }
                            }
                          },
                          {
                            "relationship": {
                              "name": "role_policies",
                              "predicate": {
                                "relationship": {
                                  "name": "role",
                                  "predicate": {
                                    "relationship": {
                                      "name": "user_roles",
                                      "predicate": {
                                        "relationship": {
                                          "name": "user",
                                          "predicate": {
                                            "relationship": {
                                              "name": "keycloak",
                                              "predicate": {
                                                "fieldComparison": {
                                                  "field": "keycloak_id",
                                                  "operator": "_eq",
                                                  "value": {
                                                    "sessionVariable": "x-hasura-user-id"
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        ]
                      }
                    }
                  }
                ]
              }
            }
          }
        }
      }
    ]
  }
}
```

## How Permissions Work in v3

### 1. Permission Evaluation Flow

```
GraphQL Query → Permission Check → Query Planning → Filter Application → Data Connector Query
```

1. **GraphQL Query**: User makes a query with their role and session variables
2. **Permission Check**: Engine checks if the role has access to the model
3. **Query Planning**: Permissions are converted to filter expressions
4. **Filter Application**: Filters are merged with user's query filters
5. **Data Connector Query**: Final query with all filters is sent to the data source

### 2. Relationship Traversal

When using relationship predicates:
- The engine follows the relationship to the related model
- Applies the predicate on the related model
- Returns only parent records where the relationship predicate is satisfied
- This effectively performs a JOIN operation at the data layer

### 3. Session Variables

Session variables work similarly to v2:
- Passed in request headers (e.g., `x-hasura-user-id`)
- Can be used in `value` expressions within predicates
- Type-safe: v3 validates session variable types

## Available Predicate Types

### 1. Field Comparison
```json
{
  "fieldComparison": {
    "field": "status",
    "operator": "_eq",
    "value": {
      "literal": "published"
    }
  }
}
```

### 2. Field Is Null
```json
{
  "fieldIsNull": {
    "field": "deleted_at"
  }
}
```

### 3. Relationship
```json
{
  "relationship": {
    "name": "author",
    "predicate": {
      "fieldComparison": {
        "field": "is_active",
        "operator": "_eq",
        "value": {
          "literal": true
        }
      }
    }
  }
}
```

### 4. Boolean Operators

**AND**:
```json
{
  "and": [
    { "fieldComparison": { ... } },
    { "relationship": { ... } }
  ]
}
```

**OR**:
```json
{
  "or": [
    { "fieldComparison": { ... } },
    { "fieldIsNull": { ... } }
  ]
}
```

**NOT**:
```json
{
  "not": {
    "fieldComparison": {
      "field": "status",
      "operator": "_eq",
      "value": {
        "literal": "draft"
      }
    }
  }
}
```

## Value Expressions

### 1. Literal Values
```json
{
  "literal": "some value"
}
```

### 2. Session Variables
```json
{
  "sessionVariable": "x-hasura-user-id"
}
```

### 3. Environment Variables (Build Time)
```json
{
  "valueFromEnv": "MY_ENV_VAR"
}
```

## Migration Tips

### 1. Operator Mapping

| v2 Operator | v3 Operator | Notes |
|-------------|-------------|-------|
| `_eq` | `_eq` | Same |
| `_neq` | `_neq` | Same |
| `_gt` | `_gt` | Same |
| `_gte` | `_gte` | Same |
| `_lt` | `_lt` | Same |
| `_lte` | `_lte` | Same |
| `_in` | `_in` | Same |
| `_nin` | `_nin` | Same |
| `_is_null` | Use `fieldIsNull` | Different structure |
| `_like` | `_like` | Check data connector support |
| `_ilike` | `_ilike` | Check data connector support |

### 2. Relationship Handling

- v2 uses nested objects to traverse relationships
- v3 uses explicit `relationship` predicates with `name` and `predicate`
- Both achieve the same result - filtering based on related data

### 3. Session Variable Format

- v2: `X-Hasura-User-Id` (uppercase)
- v3: `x-hasura-user-id` (lowercase recommended)
- Both formats typically work, but lowercase is preferred in v3

## Complete Example: Blog Platform

Here's a complete example of migrating blog platform permissions:

### v2 Permissions
```yaml
# Articles table
select_permissions:
  - role: author
    permission:
      filter:
        _or:
          - author_id:
              _eq: X-Hasura-User-Id
          - collaborators:
              user_id:
                _eq: X-Hasura-User-Id
  - role: reviewer
    permission:
      filter:
        status:
          _in: [submitted, in_review]
        assigned_reviewers:
          reviewer_id:
            _eq: X-Hasura-User-Id
```

### v3 Permissions
```json
{
  "kind": "ModelPermissions",
  "version": "v1",
  "definition": {
    "modelName": "Articles",
    "permissions": [
      {
        "role": "author",
        "select": {
          "filter": {
            "or": [
              {
                "fieldComparison": {
                  "field": "author_id",
                  "operator": "_eq",
                  "value": {
                    "sessionVariable": "x-hasura-user-id"
                  }
                }
              },
              {
                "relationship": {
                  "name": "collaborators",
                  "predicate": {
                    "fieldComparison": {
                      "field": "user_id",
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
      },
      {
        "role": "reviewer",
        "select": {
          "filter": {
            "and": [
              {
                "fieldComparison": {
                  "field": "status",
                  "operator": "_in",
                  "value": {
                    "literal": ["submitted", "in_review"]
                  }
                }
              },
              {
                "relationship": {
                  "name": "assigned_reviewers",
                  "predicate": {
                    "fieldComparison": {
                      "field": "reviewer_id",
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
      }
    ]
  }
}
```

## Limitations and Considerations

### 1. Current Limitations

- **Insert/Update/Delete**: While the types exist in the codebase, full implementation for insert, update, and delete permissions is not yet complete
- **Column-level permissions**: Use TypePermissions for field-level access control
- **Aggregation permissions**: Not yet implemented in the current version

### 2. Performance Considerations

- Relationship traversals translate to JOINs at the database level
- Deep relationship chains can impact performance
- Consider indexing foreign keys used in permission filters

### 3. Best Practices

1. **Keep permissions simple**: Complex permissions can be hard to debug
2. **Use type permissions**: For field-level access control, use TypePermissions
3. **Test thoroughly**: Verify permissions work as expected with different user scenarios
4. **Monitor performance**: Watch query performance with complex relationship permissions

## Debugging Permissions

### 1. Enable Debug Logging

Set appropriate log levels to see permission evaluation:
```bash
RUST_LOG=DEBUG cargo run --bin engine
```

### 2. Test Queries

Test your permissions with different session variables:
```graphql
# In GraphiQL, set headers:
{
  "x-hasura-role": "user",
  "x-hasura-user-id": "123"
}

# Run test query
query {
  articles {
    id
    title
    author {
      name
    }
  }
}
```

### 3. Check Generated SQL

The data connector logs will show the actual filters applied, helping you understand how permissions translate to database queries.

## Conclusion

Hasura v3's permission system is powerful and flexible, supporting complex relationship-based permissions similar to v2. While the syntax has changed, the core capabilities remain, and in many ways, the new structure is more explicit and type-safe.

The key to successful migration is understanding the new predicate structure and how relationships are explicitly defined in v3. With this guide, you should be able to migrate even complex v2 permissions to v3 format.