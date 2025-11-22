---
layout: default
title: Federated Role-Based Access Control
nav_order: 6
description: "Geometric permission manifolds for federated access control"
permalink: /concepts/federated-rbac
---

# Federated Role-Based Access Control

## What is Federated RBAC?

Federated Role-Based Access Control (FRBAC) extends traditional RBAC to work across multiple organizations and domains. It uses geometric permission manifolds and cryptographic derivation paths to manage hierarchical permissions in distributed systems.

## The Core Realization

BIP32/Speck256 aren't just about cryptocurrency wallets - they're solutions to Federated Role-Based Access Control in n-dimensional permission spaces.

The "addresses" and "keys" are instantiations of a deeper abstract pattern for hierarchical delegation of authority across federated domains.

## The Abstract FRBAC Problem

```typescript
interface FederatedRBAC<T> {
  domains: FederatedDomain[];
  roles: HierarchicalRole<T>[];
  permissions: PermissionSpace<T>;
  delegation: DelegationPath<T>[];
  
  // Core operations
  canAccess(subject: T, resource: T, action: T): boolean;
  derivePermission(path: DelegationPath<T>): Permission<T>;
  verifyFederation(domain: FederatedDomain, proof: Proof<T>): boolean;
}
```

## BIP32 in Abstract FRBAC Terms

The BIP32 derivation path:
```
m / purpose' / coin_type' / account' / change / address_index
```

Is actually:
```
Root / Domain / Organization / Department / Project / Individual
     ↓        ↓             ↓           ↓         ↓           ↓
   Global  Federation    Enterprise   Division  Team      Personal
  (Icosa)    (Cube)      (Tetra)      (Face)   (Edge)    (Vertex)
```

Each level maps to a geometric structure in the permission manifold.

## Geometric Permission Manifold

Access control is modeled as a simplicial complex:

```haskell
data AccessControlComplex = AccessControlComplex {
  vertices :: Set Subject,      -- Users/agents
  edges :: Set Role,           -- Relationships 
  faces :: Set Permission,     -- Permission groups
  volumes :: Set Domain,       -- Federated domains
  incidence :: IncidenceRelation
}

-- The derivation path is a chain in this complex
type DerivationPath = [Simplex]  -- Path through the access control complex
```

## Geometric RBAC Implementation

```typescript
class GeometricRBAC<T> {
  private topology: AccessControlComplex<T>;
  
  // Check access using geometric incidence relations
  async checkAccess(
    subject: Subject<T>,
    resource: Resource<T>, 
    action: Action<T>
  ): Promise<AccessGrant> {
    
    // Find the minimal simplex containing subject and resource
    const accessSimplex = this.findMinimalAccessSimplex(subject, resource);
    
    // Verify geometric constraints (Betti numbers, etc.)
    const geometricValid = this.verifyGeometricConstraints(accessSimplex);
    
    if (!geometricValid) {
      return {granted: false, reason: "Geometric constraints violated"};
    }
    
    // Check permission using BIP32-style derivation
    const permissionPath = this.derivePermissionPath(subject, resource);
    const permission = this.derivePermission(permissionPath);
    
    return {
      granted: permission.includes(action),
      permission: permission,
      path: permissionPath,
      proof: this.generateGeometricProof(accessSimplex, permission)
    };
  }
  
  // Derive permission using BIP32-style path
  derivePermission(path: DelegationPath<T>): Permission<T> {
    // Each level in path corresponds to geometric structure
    // Root → Domain → Organization → Department → Project → Individual
    return path.reduce((perm, level) => {
      return this.combinePermissions(perm, level.permissions);
    }, ROOT_PERMISSIONS);
  }
}
```

## Speck256 for Lightweight Encryption

Speck256 solves lightweight hierarchical encryption in resource-constrained federated environments:

- Multiple organizational boundaries
- Hierarchical trust relationships
- Limited computational resources
- Need for cross-domain verification

This makes it ideal for:
- IoT devices
- Mesh networks
- Distributed systems
- Edge computing

## Permission Derivation Path

The derivation path encodes the permission hierarchy:

```
m / domain / organization / department / project / individual
```

Each level:
1. **Domain**: Federated domain identifier
2. **Organization**: Organization within domain
3. **Department**: Department within organization
4. **Project**: Project within department
5. **Individual**: Individual user/agent

Permissions are derived by combining permissions at each level.

## Geometric Verification

Access control uses geometric verification:

1. **Betti Number Check**: Verify no cycles in permission graph (β₁ = 0)
2. **Simplicial Complex Validation**: Ensure valid geometric structure
3. **Path Verification**: Cryptographically verify derivation path
4. **Geometric Proof**: Generate proof certificate

## Use Cases

1. **Multi-Organization Collaboration**: Share resources across organizational boundaries
2. **IoT Device Management**: Hierarchical permissions for device networks
3. **Cloud Federation**: Cross-cloud access control
4. **Blockchain Applications**: Permission management in decentralized systems

## Security Properties

- **Hierarchical Isolation**: Permissions at one level don't leak to others
- **Cryptographic Verification**: All paths are cryptographically verifiable
- **Geometric Constraints**: Mathematical guarantees prevent permission errors
- **Federation Support**: Works across trust boundaries

## References

- [Federated RBAC Document](../dev-docs/research/docs/Federated Role-Based Access Control.md) - Complete specification
- [Geometric Consensus](geometric-consensus.md) - Geometric structures for consensus
- [Crypto Guide](../CRYPTO_GUIDE.md) - Cryptographic foundations

