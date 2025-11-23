---
layout: default
title: Consensus Mechanisms
nav_order: 2
description: "Consensus algorithms and mechanisms in meta-log"
permalink: /architecture/consensus-mechanisms
---

# Consensus Mechanisms

## Overview

meta-log uses geometric consensus mechanisms that replace traditional quorum-based systems (Raft, Paxos) with mathematically verifiable polyhedral thresholds.

## Geometric Consensus

### Core Principle
Each consensus decision uses a geometric shape (polyhedron/polytope) where:
- **Vertices** = Decision criteria, nodes, or stakeholders
- **Threshold** = Minimum vertices that must agree
- **Duality** = Encodes semantic meaning

### Consensus Priority Order

When multiple mechanisms are available, priority is:
1. **Branch Cut** (UTCT) - Uniqueness proof
2. **Harmony** (UTCT) - Mathematical consistency
3. **Perceptron** - Learned strategy
4. **Fano** - Perfect match
5. **Lottery** - 2-of-3 voting
6. **LWW** - Timestamp fallback

## Context-Specific Mechanisms

### Private/Local Context
- **Tetrahedron**: 4/4 (100%) - MUST_LOCAL
- **Octahedron**: 5/6 (83.3%) - SHOULD_LOCAL
- **Cube**: 4/8 (50%) - MAY_LOCAL

### Protected/Federated Context
- **5-cell**: 5/5 (100%) - MUST_PROTECTED
- **24-cell**: 20/24 (83.3%) - SHOULD_PROTECTED
- **8-cell**: 8/16 (50%) - MAY_PROTECTED

### Public/Global Context
- **Truncated Tetrahedron**: 12/12 (100%) - MUST_PUBLIC
- **Cuboctahedron**: 10/12 (83.3%) - SHOULD_PUBLIC
- **Truncated Cube**: 12/24 (50%) - MAY_PUBLIC

## Network Partition Handling

Geometric consensus naturally handles partitions:
- **Detection**: Betti number β₀ > 1
- **Decomposition**: Automatic shape decomposition
- **Recovery**: Dual-based state mapping

See [Network Partitions](../NETWORK_PARTITIONS.md) for details.

## Comparison with Traditional Systems

| Aspect | Raft/Paxos | Geometric Consensus |
|--------|------------|---------------------|
| **Threshold** | Majority (N/2 + 1) | Geometric (varies: 50%, 83%, 90%, 100%) |
| **Semantics** | None (just agreement) | Duality-encoded (permission ↔ recommendation) |
| **Verification** | Runtime consensus | Mathematical proof certificates |
| **Partition Detection** | Heartbeat timeout | β₀ > 1 (topological, O(v)) |
| **Recovery** | Leader election | Dual mapping |

## Implementation

```typescript
class GeometricConsensus {
  async verify(
    requirement: Requirement,
    context: Context,
    vertices: DecisionVertex[]
  ): Promise<ConsensusCertificate> {
    // 1. Select geometric constraint
    const constraint = selectConstraint(requirement, context);
    
    // 2. Verify vertex count
    assert(vertices.length === constraint.vertices);
    
    // 3. Count agreements
    const agrees = vertices.filter(v => v.agrees).length;
    const required = Math.ceil(constraint.vertices * constraint.threshold);
    
    // 4. Generate proof
    const proof = generateProof(constraint, agrees, required);
    
    return {
      valid: agrees >= required,
      proof,
      certificate: signCertificate(proof)
    };
  }
}
```

## References

- [Geometric Consensus](../GEOMETRIC_CONSENSUS.md) - Complete overview
- [Network Partitions](../NETWORK_PARTITIONS.md) - Partition handling
- [Geometric Architecture](geometric-architecture.md) - System architecture

