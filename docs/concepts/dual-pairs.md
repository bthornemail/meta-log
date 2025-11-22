---
layout: default
title: Dual Pairs in Computational Scheme Theory
nav_order: 5
description: "Unified categorical framework using dual pairs for computational structures"
permalink: /concepts/dual-pairs
---

# Dual Pairs in Computational Scheme Theory

## What are Dual Pairs?

Dual pairs are a fundamental mathematical concept where two structures are related by exchanging certain properties. In computational scheme theory, dual pairs provide a unified categorical framework for understanding how different computational structures relate to each other.

## The Core Principle

Dual pairs encode complementary perspectives:
- **Left side**: Affine space, GCD (Greatest Common Divisor), "what things ARE"
- **Right side**: Projective space, LCM (Least Common Multiple), "what things DO"

This duality appears throughout computational structures:
- Data structures ↔ Algorithms
- Types ↔ Operations
- States ↔ Transitions
- Values ↔ Functions

## CanvasL Dual-Pair Stratification

The CanvasL format uses dual-pair stratification to organize computational structures:

### Left Side (Affine/GCD)
- Pure value layer
- Ports, 8-tuple, Environment
- Wallet → Address → Blockchain
- "What things ARE"

### Right Side (Projective/LCM)
- Pure action layer
- Consumer ↔ Provider
- Order ↔ Fulfillment
- Publish/Subscribe/Chat/Trade
- "What things DO"

### The Rule

**NEVER** mix left and right sides in the same file. Each CanvasL file must be pure:
- `<file1>.canvasl` → pure left side (affine/GCD)
- `<file2>.canvasl` → pure right side (projective/LCM)

This maintains geometric integrity and enables mathematical verification.

## Geometric Duality in Consensus

Dual pairs encode semantic meaning in geometric consensus:

### Self-Dual Structures
- **Tetrahedron**: Self-dual → "What it IS" = "What it MUST BE"
- **24-cell**: Self-dual → Internal consistency

### Dual Pairs
- **Cube ↔ Octahedron**: Permission ↔ Recommendation
- **8-cell ↔ 16-cell**: Permission in federation ↔ Strong recommendation
- **Dodecahedron ↔ Icosahedron**: Prohibition ↔ Alternative

### Duality Mapping

When structures are dual:
- Vertices of one = Faces of the other
- Edges are preserved
- Semantic meaning is inverted/complementary

## Categorical Framework

Dual pairs form a category where:
- **Objects**: Computational structures
- **Morphisms**: Dual relationships
- **Composition**: Chain of dual transformations
- **Identity**: Self-dual structures

This provides a mathematical foundation for:
- Type systems
- Program transformations
- State transitions
- Consensus mechanisms

## Applications

### 1. Type System Design
Dual pairs relate:
- Product types ↔ Sum types
- Covariant types ↔ Contravariant types
- Data structures ↔ Algorithms

### 2. Program Transformation
Dual pairs enable:
- Forward evaluation ↔ Backward evaluation
- Compilation ↔ Decompilation
- Optimization ↔ Deoptimization

### 3. State Management
Dual pairs connect:
- Current state ↔ State transitions
- Values ↔ Operations
- Storage ↔ Computation

## Mathematical Properties

### Duality Invariance
If A and B are dual, then:
- Properties of A map to complementary properties of B
- Operations on A have dual operations on B
- Proofs about A translate to proofs about B

### Composition
Dual relationships compose:
```
If A ↔ B (dual) and B ↔ C (dual), then A ↔ C (dual)
```

### Preservation
Dual pairs preserve:
- Structural properties
- Computational complexity
- Semantic meaning (inverted)

## References

- [Geometric Consensus](geometric-consensus.md) - How duality encodes consensus semantics
- [Network Partitions](network-partitions.md) - Using duality for partition recovery
- [Dual Pairs PDF](../dev-docs/research/docs/Dual Pairs in Computational Scheme Theory_ A Unified Categorical Framework.PDF) - Complete mathematical framework

