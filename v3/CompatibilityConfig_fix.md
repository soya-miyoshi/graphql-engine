# Fix for CompatibilityConfig Error

The issue is that `CompatibilityConfig` is not a valid object kind for the
`subgraphs.objects` array in v3 metadata.

Based on the codebase analysis:

1. **CompatibilityConfig should be defined at the metadata root level**, not
   inside subgraphs
2. **For v3 metadata**, compatibility is controlled via the `flags` field at the
   root level

## Solution 1: Remove CompatibilityConfig from subgraphs

Simply remove this object from your metadata:

```json
{
  "date": "2024-12-18",
  "kind": "CompatibilityConfig"
}
```

## Solution 2: Use proper v3 metadata structure with flags

If you need compatibility settings, use the v3 metadata format:

```json
{
  "version": "v3",
  "subgraphs": [
    {
      "name": "globals",
      "objects": [
        // Your objects here (WITHOUT CompatibilityConfig)
      ]
    }
  ],
  "flags": {
    // Compatibility flags go here if needed
  }
}
```

## The Correct Metadata Structure

Your metadata should look like this:

```json
{
  "version": "v3", // Add this at the root
  "subgraphs": [
    {
      "name": "globals",
      "objects": [
        {
          "definition": {
            "mode": {
              "noAuth": {
                "role": "admin",
                "sessionVariables": {}
              }
            }
          },
          "kind": "AuthConfig",
          "version": "v3"
        },
        // Remove CompatibilityConfig from here
        {
          "definition": {
            "apolloFederation": null,
            "mutation": {
              "rootOperationTypeName": "Mutation"
            }
            // ... rest of GraphqlConfig
          },
          "kind": "GraphqlConfig",
          "version": "v1"
        }
        // ... other valid objects
      ]
    }
  ]
}
```

## Valid Object Kinds for Subgraphs

Based on the OpenDdSubgraphObject enum, these are the only valid kinds:

- DataConnectorLink
- GraphqlConfig
- ObjectType
- ScalarType
- ObjectBooleanExpressionType
- BooleanExpressionType
- OrderByExpression
- DataConnectorScalarRepresentation
- AggregateExpression
- Model
- Command
- Relationship
- TypePermissions
- ModelPermissions
- CommandPermissions
- Apollo

`CompatibilityConfig` is NOT in this list, which is why you're getting the
error.
