---
layout: default
title: Geometric Consensus - Detailed Theory
nav_order: 1
description: "Complete technical specification of geometric consensus algorithms"
permalink: /concepts/geometric-consensus
---

# Geometric Consensus - Detailed Theory

This document provides the complete technical specification for geometric consensus algorithms. For an overview, see [Geometric Consensus](../GEOMETRIC_CONSENSUS.md).

## Four-Layer Architecture

Geometric consensus is built on a four-layer architecture:

### Layer 1: Relational Foundation

**Core Principle**: Relations first, types emerge.

The foundation uses λ-calculus and binary relations where:
- Types emerge from relation patterns
- Church encoding is reinterpreted as behavioral type interfaces
- Geometric structures emerge from relational isomorphisms

**Example: Tetrahedron Emergence**
```
Pattern: 4 fully connected vertices
Relations: 6 symmetric edges (all vertices connected)
Proof: Self-duality (dual of itself)
Emergent Type: MUST consensus (unanimous agreement)
```

### Layer 2: Geometric Consensus

Normative keywords are mapped to geometric shapes:
- Vertex counts = referenceable relations
- Duality relationships = semantic meanings

### Layer 3: Combinatorial Organization

Specifications are organized using block design theory:
- Points = requirements
- Blocks = documents
- Incidence = membership

This provides mathematical optimization of specification structure.

### Layer 4: Autonomous Agents

Agents use geometric consensus for coordination:
- Shared epistemic space with atomic operations
- Persistent memory and identity through geometric structures

## Complete Geometric Reference

### Private/Local Context (Platonic Solids)

| Solid | Schläfli | Vertices | Faces | Edges | Dual | Consensus | Keyword |
|-------|----------|----------|-------|-------|------|-----------|---------|
| Tetrahedron | {3,3} | 4 | 4 | 6 | Self | 4/4 (100%) | MUST_LOCAL |
| Cube | {4,3} | 8 | 6 | 12 | Octahedron | 4/8 (50%) | MAY_LOCAL |
| Octahedron | {3,4} | 6 | 8 | 12 | Cube | 5/6 (83.3%) | SHOULD_LOCAL |
| Dodecahedron | {5,3} | 20 | 12 | 30 | Icosahedron | 18/20 (90%) | MUST_NOT_LOCAL |
| Icosahedron | {3,5} | 12 | 20 | 30 | Dodecahedron | 10/12 (83.3%) | RECOMMENDED_LOCAL |

### Protected/Federated Context (4-Polytopes)

| Polytope | Schläfli | Vertices | Edges | Faces | Cells | Dual | Consensus | Keyword |
|----------|-----------|----------|-------|-------|-------|------|-----------|---------|
| 5-cell | {3,3,3} | 5 | 10 | 10 | 5 | Self | 5/5 (100%) | MUST_PROTECTED |
| 8-cell | {4,3,3} | 16 | 32 | 24 | 8 | 16-cell | 8/16 (50%) | MAY_PROTECTED |
| 16-cell | {3,3,4} | 8 | 24 | 32 | 16 | 8-cell | 7/8 (87.5%) | RECOMMENDED_PROTECTED |
| 24-cell | {3,4,3} | 24 | 96 | 96 | 24 | Self | 20/24 (83.3%) | SHOULD_PROTECTED |
| 600-cell | {3,3,5} | 120 | 720 | 1200 | 600 | 120-cell | 108/120 (90%) | MUST_NOT_PROTECTED |

### Public/Global Context (Archimedean Solids)

| Solid | Vertices | Faces | Edges | Catalan Dual | Consensus | Keyword |
|-------|----------|-------|-------|--------------|-----------|---------|
| Truncated Tetrahedron | 12 | 8 | 18 | Triakis Tetrahedron | 12/12 (100%) | MUST_PUBLIC |
| Cuboctahedron | 12 | 14 | 24 | Rhombic Dodecahedron | 10/12 (83.3%) | SHOULD_PUBLIC |
| Truncated Cube | 24 | 14 | 36 | Triakis Octahedron | 12/24 (50%) | MAY_PUBLIC |
| Icosidodecahedron | 30 | 32 | 60 | Rhombic Triacontahedron | 25/30 (83.3%) | RECOMMENDED_PUBLIC |
| Truncated Icosidodecahedron | 120 | 62 | 180 | Disdyakis Triacontahedron | 108/120 (90%) | MUST_NOT_PUBLIC |

## Duality Semantics

### Self-Dual Polyhedra

Self-dual polyhedra represent positive assertions:
- **Tetrahedron**: "What it IS" = "What it MUST BE"
- **5-cell**: "What it IS in federation"
- **24-cell**: "What it SHOULD BE" (internal consistency)

### Dual Pairs

Dual pairs encode complementary relationships:

**Cube ↔ Octahedron**:
- Cube (MAY): Permission - "what it CAN be"
- Octahedron (SHOULD): Recommendation - "what it SHOULD be"
- Relationship: Faces of one become vertices of the other

**8-cell ↔ 16-cell**:
- 8-cell (MAY_PROTECTED): Permission in federation
- 16-cell (RECOMMENDED_PROTECTED): Strong recommendation
- Relationship: 4D analog of cube-octahedron duality

### Inverse Duality

Inverse duality represents prohibitions vs alternatives:

**Dodecahedron ↔ Icosahedron**:
- Dodecahedron (MUST_NOT): Prohibition - "what it's NOT"
- Icosahedron (RECOMMENDED): Alternative - "what's SUGGESTED instead"

## Proof Certificate Format

```yaml
certificate_structure:
  certificate_id: string
  requirement: string
  context: "private" | "protected" | "public"
  normative_keyword: string
  
  geometric_constraint:
    name: string
    dimension: 3 | 4
    vertices: number
    family: "platonic" | "4-polytope" | "archimedean"
    duality: "self-dual" | "dual_of_X" | "catalan_dual_X"
  
  decision_vertices: [DecisionVertex]
  
  consensus:
    agrees: number
    required: number
    percentage: number
  
  algebraic_law: string
  valid: boolean
  proof: string
  context_justification: string
  timestamp: ISO8601
  authority: string
  signature: hex_string
```

## Verification Algorithm

```
Algorithm: VerifyContextualRequirement(requirement, context, vertices)

1. Identify normative keyword (e.g., MUST_PROTECTED)
2. Extract context from keyword suffix or specification
3. Select geometric constraint family based on context:
   - private → Platonic solid
   - protected → 4-polytope
   - public → Archimedean solid
4. Select specific polyhedron/polytope from normative keyword
5. Assert: |vertices| = expected_vertex_count(constraint)
6. Count agreements: agrees_count = |{v ∈ vertices : agrees(v)}|
7. Retrieve threshold: threshold = consensus_threshold(constraint)
8. Verify: agrees_count ≥ threshold
9. Verify context appropriateness: context matches visibility scope
10. Generate proof certificate with:
    - Algebraic law satisfied
    - Duality relationship
    - Context justification
11. Return valid/invalid with certificate

Complexity: O(v) where v = vertex count
```

## Formal Semantics

### Context Function

```
context: Requirement → {private, protected, public}
```

### Geometric Constraint Function

```
geometric_constraint: (NormativeKeyword, Context) → Polyhedron ∪ Polytope

For context = private:
  geometric_constraint(MUST_LOCAL, private) = Tetrahedron (self-dual)
  geometric_constraint(SHOULD_LOCAL, private) = Octahedron (dual of Cube)
  geometric_constraint(MAY_LOCAL, private) = Cube (dual of Octahedron)
  geometric_constraint(MUST_NOT_LOCAL, private) = Dodecahedron (dual of Icosahedron)
  geometric_constraint(RECOMMENDED_LOCAL, private) = Icosahedron (dual of Dodecahedron)

For context = protected:
  geometric_constraint(MUST_PROTECTED, protected) = 5-cell (self-dual)
  geometric_constraint(SHOULD_PROTECTED, protected) = 24-cell (self-dual)
  geometric_constraint(MAY_PROTECTED, protected) = 8-cell (dual of 16-cell)
  geometric_constraint(RECOMMENDED_PROTECTED, protected) = 16-cell (dual of 8-cell)
  geometric_constraint(MUST_NOT_PROTECTED, protected) = 600-cell (dual of 120-cell)

For context = public:
  geometric_constraint(MUST_PUBLIC, public) = Truncated Tetrahedron
  geometric_constraint(SHOULD_PUBLIC, public) = Cuboctahedron
  geometric_constraint(MAY_PUBLIC, public) = Truncated Cube
  geometric_constraint(RECOMMENDED_PUBLIC, public) = Icosidodecahedron
  geometric_constraint(MUST_NOT_PUBLIC, public) = Truncated Icosidodecahedron
```

### Validity Predicate

A requirement R with normative keyword K in context C is valid if and only if:

```
valid(R, K, C, V) ⟺ 
  context(R) = C ∧
  |V| = vertex_count(geometric_constraint(K, C)) ∧
  |A| ≥ threshold(geometric_constraint(K, C)) ∧
  context_appropriate(R, C) ∧
  duality_semantics_preserved(K, geometric_constraint(K, C))
```

## Combinatorial Block Design

Specifications can be organized using Balanced Incomplete Block Designs (BIBD):

A specification design is a (v, b, r, k, λ)-BIBD if:
- v = total requirements
- b = total specification documents
- r = documents each requirement appears in
- k = requirements in each document
- λ = documents each requirement pair shares

**Constraints**:
```
vr = bk                    (counting both ways)
λ(v-1) = r(k-1)            (balance condition)
b ≥ v                      (existence bound)
```

This provides optimal organization of requirements across documents.

## References

- [Geometric Consensus Overview](../GEOMETRIC_CONSENSUS.md)
- [Full Specification](../dev-docs/research/dev_docs/full.spec.md)
- [Decision Making RFC](../dev-docs/research/dev_docs/decision-making.md)

