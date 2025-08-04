# Metadata Tooling Guide for Hasura v3

You're absolutely right that v3's JSON format is much more verbose than v2's YAML. This guide covers the available tools and approaches to make metadata creation easier in v3.

## Current State of Tooling

### What's Missing from v2
- ❌ **No Console UI**: v3 doesn't have a web console like v2 for visually creating permissions
- ❌ **No Point-and-Click**: The graphical permission builder from v2 console is not available

### What's Available in v3

#### 1. **Hasura DDN CLI** (Proprietary)
The official way to work with v3 metadata is through Hasura's Data Delivery Network (DDN) CLI:

```bash
# Install DDN CLI
curl -L https://graphql-engine-cdn.hasura.io/ddn/cli/latest/install.sh | bash

# Generate basic metadata
ddn model add <connector-name> "*"
ddn command add <connector-name> "*"
ddn relationship add <connector-name> "*"
```

**Limitations**:
- Only generates basic metadata with `admin` role permissions
- All other permissions must be added manually
- DDN CLI is proprietary (not open source)

#### 2. **VS Code Extension**
Hasura provides a VS Code extension for v3 that offers:
- Syntax highlighting for metadata files
- Auto-completion
- Schema validation
- Inline documentation

#### 3. **Manual Approaches & Helper Scripts**

Since the metadata is JSON, you can create helper scripts to generate permissions. Here's a simple Node.js example:

```javascript
// permission-generator.js
class PermissionBuilder {
  constructor(modelName) {
    this.metadata = {
      kind: "ModelPermissions",
      version: "v1",
      definition: {
        modelName: modelName,
        permissions: []
      }
    };
  }

  addRole(roleName) {
    this.currentRole = {
      role: roleName,
      select: {
        filter: null,
        argument_presets: [],
        allow_subscriptions: false
      }
    };
    this.metadata.definition.permissions.push(this.currentRole);
    return this;
  }

  whereField(field, operator, value) {
    this.currentRole.select.filter = {
      fieldComparison: {
        field: field,
        operator: operator,
        value: value
      }
    };
    return this;
  }

  whereSessionVariable(field, operator, sessionVar) {
    return this.whereField(field, operator, {
      sessionVariable: sessionVar
    });
  }

  whereRelationship(relationshipName, predicate) {
    this.currentRole.select.filter = {
      relationship: {
        name: relationshipName,
        predicate: predicate
      }
    };
    return this;
  }

  orConditions(...conditions) {
    this.currentRole.select.filter = {
      or: conditions
    };
    return this;
  }

  andConditions(...conditions) {
    this.currentRole.select.filter = {
      and: conditions
    };
    return this;
  }

  build() {
    return JSON.stringify(this.metadata, null, 2);
  }
}

// Usage example - recreating your complex permission
const builder = new PermissionBuilder("YourModelName");

const permission = builder
  .addRole("user")
  .orConditions(
    {
      relationship: {
        name: "space",
        predicate: {
          relationship: {
            name: "user",
            predicate: {
              relationship: {
                name: "keycloak",
                predicate: {
                  fieldComparison: {
                    field: "keycloak_id",
                    operator: "_eq",
                    value: {
                      sessionVariable: "x-hasura-user-id"
                    }
                  }
                }
              }
            }
          }
        }
      }
    },
    {
      relationship: {
        name: "space",
        predicate: {
          relationship: {
            name: "policies",
            predicate: {
              and: [
                {
                  fieldComparison: {
                    field: "policy_write_this_space",
                    operator: "_eq",
                    value: { literal: true }
                  }
                },
                {
                  relationship: {
                    name: "role_policies",
                    predicate: {
                      relationship: {
                        name: "role",
                        predicate: {
                          relationship: {
                            name: "user_roles",
                            predicate: {
                              relationship: {
                                name: "user",
                                predicate: {
                                  relationship: {
                                    name: "keycloak",
                                    predicate: {
                                      fieldComparison: {
                                        field: "keycloak_id",
                                        operator: "_eq",
                                        value: {
                                          sessionVariable: "x-hasura-user-id"
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
      }
    }
  )
  .build();

console.log(permission);
```

## Alternative Approaches

### 1. **YAML to JSON Conversion**
You can write permissions in YAML (which is less verbose) and convert to JSON:

```yaml
# permissions.yaml
kind: ModelPermissions
version: v1
definition:
  modelName: Articles
  permissions:
    - role: user
      select:
        filter:
          relationship:
            name: author
            predicate:
              fieldComparison:
                field: id
                operator: _eq
                value:
                  sessionVariable: x-hasura-user-id
```

Convert using a script:
```bash
# Using Python
python -c "import yaml, json; print(json.dumps(yaml.safe_load(open('permissions.yaml')), indent=2))"

# Using Node.js with js-yaml
npx js-yaml permissions.yaml > permissions.json
```

### 2. **TypeScript Type Definitions**
Create TypeScript interfaces for type-safe permission building:

```typescript
interface ModelPermissions {
  kind: "ModelPermissions";
  version: "v1";
  definition: {
    modelName: string;
    permissions: Permission[];
  };
}

interface Permission {
  role: string;
  select?: SelectPermission;
}

interface SelectPermission {
  filter: Predicate | null;
  argument_presets?: ArgumentPreset[];
  allow_subscriptions?: boolean;
}

type Predicate = 
  | { fieldComparison: FieldComparison }
  | { fieldIsNull: { field: string } }
  | { relationship: RelationshipPredicate }
  | { and: Predicate[] }
  | { or: Predicate[] }
  | { not: Predicate };

// Use with autocomplete in your IDE
const permission: ModelPermissions = {
  kind: "ModelPermissions",
  version: "v1",
  definition: {
    modelName: "Articles",
    permissions: [
      {
        role: "user",
        select: {
          filter: {
            fieldComparison: {
              field: "author_id",
              operator: "_eq",
              value: {
                sessionVariable: "x-hasura-user-id"
              }
            }
          }
        }
      }
    ]
  }
};
```

### 3. **Template-Based Generation**
Create reusable permission templates:

```javascript
// templates.js
const templates = {
  // Owner-based permission
  ownerOnly: (modelName, ownerField) => ({
    kind: "ModelPermissions",
    version: "v1",
    definition: {
      modelName,
      permissions: [{
        role: "user",
        select: {
          filter: {
            fieldComparison: {
              field: ownerField,
              operator: "_eq",
              value: { sessionVariable: "x-hasura-user-id" }
            }
          }
        }
      }]
    }
  }),

  // Organization-based permission
  organizationMember: (modelName, orgRelationship) => ({
    kind: "ModelPermissions",
    version: "v1",
    definition: {
      modelName,
      permissions: [{
        role: "user",
        select: {
          filter: {
            relationship: {
              name: orgRelationship,
              predicate: {
                relationship: {
                  name: "members",
                  predicate: {
                    fieldComparison: {
                      field: "user_id",
                      operator: "_eq",
                      value: { sessionVariable: "x-hasura-user-id" }
                    }
                  }
                }
              }
            }
          }
        }
      }]
    }
  })
};

// Usage
const articlePermissions = templates.ownerOnly("Articles", "author_id");
const projectPermissions = templates.organizationMember("Projects", "organization");
```

## Community Tools

While there's no official visual permission builder for v3 yet, the community might develop tools. Keep an eye on:

1. **GitHub Discussions**: Check the [graphql-engine](https://github.com/hasura/graphql-engine/discussions) repository
2. **Community Projects**: Search for "hasura v3 metadata generator" tools
3. **VS Code Extensions**: Beyond the official one, community extensions might emerge

## Best Practices for Managing v3 Metadata

### 1. **Use Version Control**
```bash
# Organize metadata files
metadata/
├── models/
│   ├── articles-permissions.json
│   ├── users-permissions.json
│   └── ...
├── relationships/
└── types/
```

### 2. **Create Snippets**
In VS Code, create snippets for common permission patterns:

```json
{
  "Owner Permission": {
    "prefix": "perm-owner",
    "body": [
      "{",
      "  \"fieldComparison\": {",
      "    \"field\": \"${1:owner_id}\",",
      "    \"operator\": \"_eq\",",
      "    \"value\": {",
      "      \"sessionVariable\": \"x-hasura-user-id\"",
      "    }",
      "  }",
      "}"
    ]
  }
}
```

### 3. **Validation Scripts**
Create scripts to validate your permissions:

```javascript
// validate-permissions.js
const fs = require('fs');

function validatePermissions(filePath) {
  const metadata = JSON.parse(fs.readFileSync(filePath));
  
  // Check for required fields
  if (!metadata.kind || metadata.kind !== 'ModelPermissions') {
    throw new Error('Invalid kind');
  }
  
  // Check for duplicate roles
  const roles = new Set();
  for (const perm of metadata.definition.permissions) {
    if (roles.has(perm.role)) {
      throw new Error(`Duplicate role: ${perm.role}`);
    }
    roles.add(perm.role);
  }
  
  console.log('✅ Permissions valid!');
}
```

## Migration Helper

For migrating from v2 to v3, you could create a converter:

```javascript
// v2-to-v3-converter.js
function convertV2PermissionToV3(v2Permission, modelName) {
  function convertPredicate(v2Predicate) {
    // Handle different v2 operators
    if (v2Predicate._eq !== undefined) {
      return {
        fieldComparison: {
          field: Object.keys(v2Predicate)[0],
          operator: "_eq",
          value: convertValue(v2Predicate._eq)
        }
      };
    }
    
    if (v2Predicate._and) {
      return {
        and: v2Predicate._and.map(convertPredicate)
      };
    }
    
    if (v2Predicate._or) {
      return {
        or: v2Predicate._or.map(convertPredicate)
      };
    }
    
    // Handle nested relationships
    // ... more conversion logic
  }
  
  return {
    kind: "ModelPermissions",
    version: "v1",
    definition: {
      modelName,
      permissions: [
        {
          role: v2Permission.role,
          select: {
            filter: convertPredicate(v2Permission.permission.filter)
          }
        }
      ]
    }
  };
}
```

## Future Outlook

Based on the codebase and documentation:

1. **DDN Console**: Hasura is likely working on a visual console for DDN/v3
2. **Open Source Tools**: As v3 adoption grows, expect community tools
3. **AI Assistants**: Tools like GitHub Copilot can help generate v3 metadata based on patterns

## Recommendation

For now, the best approach is:

1. **Use DDN CLI** for initial metadata generation (if you have access)
2. **Create helper scripts** for your specific permission patterns
3. **Use YAML** for authoring and convert to JSON
4. **Build a library** of common permission templates
5. **Use VS Code** with the Hasura extension for better authoring experience

While not as convenient as v2's console, these approaches can significantly reduce the verbosity and complexity of creating v3 permissions.