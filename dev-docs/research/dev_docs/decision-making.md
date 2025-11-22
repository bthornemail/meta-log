# RFC XXXX: Geometric Normative Keywords for Multi-Context Consensus Decision Making

## Abstract

This specification extends RFC 2119's normative keywords (MUST, SHOULD, MAY, etc.) with explicit geometric consensus constraints represented by regular and semi-regular polyhedra across three visibility contexts: private/local, protected/intermediate, and public/shared. The geometric families are chosen for their vertex count ranges, which correspond to the number of referenceable relations (decision criteria) at each scope. Duality relationships between polyhedra encode semantic meanings about positive assertions, permissions, recommendations, and prohibitions. This framework enables formal verification of decision validity through algebraic proofs and provides unambiguous consensus requirements for multi-level distributed systems.

## Status of This Memo

This document specifies a proposed standard for the Internet community, and requests discussion and suggestions for improvements.

## Copyright Notice

Copyright (C) 2025. All Rights Reserved.

---

## 1. Introduction

### 1.1. Motivation

RFC 2119 defines normative keywords (MUST, SHOULD, MAY) that indicate requirement levels in specifications. However, these keywords lack:
1. Explicit consensus thresholds for distributed decision-making
2. Contextual scoping for different visibility levels (private, protected, public)
3. Formal verification mechanisms for requirement satisfaction

This specification addresses these gaps by mapping normative keywords to geometric structures across three contexts, where vertex counts represent the number of referenceable relations (decision criteria, nodes, or stakeholders) involved in each decision type.

### 1.2. Terminology

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in RFC 2119, with the additional geometric and contextual constraints defined herein.

**Additional Terms:**

- **Decision Vertex**: A single criterion, node, or stakeholder in a consensus decision
- **Referenceable Relation**: A distinct decision criterion that can be meaningfully tracked
- **Consensus Threshold**: The minimum proportion of vertices that must agree
- **Geometric Constraint**: The polyhedron/polytope that defines consensus rules through its vertex count
- **Visibility Context**: The scope of information sharing (private, protected, public)
- **Proof Certificate**: A compact algebraic proof that a requirement is satisfied
- **Polytope**: A geometric object with flat sides (generalizes polyhedra to higher dimensions)
- **Duality**: A geometric relationship where vertices and faces are exchanged; used to encode semantic meaning

### 1.3. Why Geometric Families?

This specification uses three distinct geometric families based on two principles:

#### 1.3.1. Vertex Count Scaling Principle

Different visibility contexts involve different scales of referenceable relations:

**Small Scale (4-20 relations)**: Platonic Solids
- Local decisions with bounded criteria
- Single-node processing with limited coordination needs
- Example: Input validation with 4-20 distinct checks

**Medium Scale (5-120 relations)**: Regular 4-Polytopes
- Federated decisions across multiple nodes
- Cross-boundary coordination requiring more granularity
- Vertex counts (5, 8, 16, 24, 120) fill gaps between Platonic and Archimedean ranges
- Example: Cluster consensus with 5-120 participating nodes

**Large Scale (12-120 relations)**: Archimedean Solids
- Public decisions with many diverse stakeholders
- Multi-organization coordination requiring flexibility
- 13 different solids provide MORE OPTIONS for precise stakeholder matching
- Example: Standards with 12-120 distinct stakeholder criteria

#### 1.3.2. Duality Semantics Principle

Polyhedra are selected based on duality relationships that encode semantic meaning:

**Self-Dual Polyhedra**: Represent positive assertions (MUST, SHOULD)
- The polyhedron is its own dual
- Represents unity: "what it IS" = "what it MUST BE"
- Examples: Tetrahedron (MUST), 24-cell (SHOULD in protected context)

**Dual Pairs**: Represent complementary assertions
- Cube ↔ Octahedron: Permission (MAY) ↔ Recommendation (SHOULD)
- Faces of one become vertices of the other
- Encodes relationship between "what it CAN be" and "what it SHOULD be"

**Inverse Duality**: Represent prohibitions vs alternatives
- Dodecahedron ↔ Icosahedron: Prohibition (MUST_NOT) ↔ Recommendation
- Encodes relationship between "what it's NOT" and "what's SUGGESTED instead"

### 1.4. Visibility Contexts

This specification defines three visibility contexts:

#### 1.4.1. Private/Local Context
**Geometric Family**: Platonic Solids (5 regular convex polyhedra in 3D)  
**Vertex Range**: 4-20  
**Used For**: Internal decisions, single-node processing, local state management

| Polyhedron    | Schläfli | Vertices | Duality      | Semantic Role |
|---------------|----------|----------|--------------|---------------|
| Tetrahedron   | {3,3}    | 4        | Self-dual    | MUST (what it IS) |
| Cube          | {4,3}    | 8        | Dual of Octahedron | MAY (what it CAN be) |
| Octahedron    | {3,4}    | 6        | Dual of Cube | SHOULD (what it SHOULD be) |
| Dodecahedron  | {5,3}    | 20       | Dual of Icosahedron | MUST_NOT (what it's NOT) |
| Icosahedron   | {3,5}    | 12       | Dual of Dodecahedron | RECOMMENDED (alternative) |

#### 1.4.2. Protected/Intermediate Context
**Geometric Family**: Regular 4-Polytopes (6 regular convex polytopes in 4D)  
**Vertex Range**: 5-120  
**Used For**: Cross-node coordination, federation boundaries, trusted subsystem communication

| Polytope      | Schläfli  | Vertices | Duality      | Semantic Role |
|---------------|-----------|----------|--------------|---------------|
| 5-cell        | {3,3,3}   | 5        | Self-dual    | MUST (what it IS in federation) |
| 8-cell        | {4,3,3}   | 16       | Dual of 16-cell | MAY (what it CAN be) |
| 16-cell       | {3,3,4}   | 8        | Dual of 8-cell | RECOMMENDED (strong suggestion) |
| 24-cell       | {3,4,3}   | 24       | Self-dual    | SHOULD (what it SHOULD BE) |
| 600-cell      | {3,3,5}   | 120      | Dual of 120-cell | MUST_NOT (prohibition) |
| 120-cell      | {5,3,3}   | 600      | Dual of 600-cell | (not used - too large) |

#### 1.4.3. Public/Shared Context
**Geometric Family**: Archimedean Solids (13 semi-regular convex polyhedra in 3D)  
**Vertex Range**: 12-120  
**Used For**: Public APIs, open protocols, multi-organization coordination

| Solid                      | Vertices | Catalan Dual           | Semantic Role |
|----------------------------|----------|------------------------|---------------|
| Truncated Tetrahedron      | 12       | Triakis Tetrahedron    | MUST (standard IS) |
| Cuboctahedron              | 12       | Rhombic Dodecahedron   | SHOULD (standard SHOULD be) |
| Truncated Cube             | 24       | Triakis Octahedron     | MAY (standard CAN be) |
| Truncated Octahedron       | 24       | Tetrakis Hexahedron    | Alternative MAY |
| Rhombicuboctahedron        | 24       | Deltoidal Icositetrahedron | Alternative MAY |
| Truncated Cuboctahedron    | 48       | Disdyakis Dodecahedron | High-complexity MAY |
| Snub Cube                  | 24       | Pentagonal Icositetrahedron | Alternative MAY |
| Icosidodecahedron          | 30       | Rhombic Triacontahedron | RECOMMENDED |
| Truncated Dodecahedron     | 60       | Triakis Icosahedron    | High-complexity MUST |
| Truncated Icosahedron      | 60       | Pentakis Dodecahedron  | High-complexity MUST |
| Rhombicosidodecahedron     | 60       | Deltoidal Hexecontahedron | Large-scale SHOULD |
| Truncated Icosidodecahedron| 120      | Disdyakis Triacontahedron | MUST_NOT |
| Snub Dodecahedron          | 60       | Pentagonal Hexecontahedron | Alternative SHOULD |

**Note on Public Context Duality**: Archimedean solids are vertex-transitive (all vertices equivalent) but their duals are Catalan solids (face-transitive). The dual represents the inverse perspective: the standard itself (Archimedean) vs those who verify compliance with it (Catalan).

---

## 2. Context-Specific Normative Keywords

### 2.1. Private/Local Context (Platonic Solids)

Used for decisions that remain within a single node, process, or organizational boundary.

#### 2.1.1. MUST_LOCAL (Tetrahedron - Self-Dual)
**Geometric Constraint**: Regular Tetrahedron  
**Schläfli Symbol**: {3,3}  
**Vertices**: 4  
**Consensus Required**: 4/4 (100%)  
**Duality**: Self-dual  
**Semantic**: Positive assertion - what it IS  
**Context**: Private/Local

**Definition**: All local decision criteria must unanimously agree. The tetrahedron's self-duality represents the unity between definition and requirement—there is no distinction between "what it is" and "what it must be."

**Example**:
```yaml
requirement: "MUST_LOCAL validate input before processing"
context: private
decision_vertices:
  vertex_1: type_correctness
  vertex_2: range_validity
  vertex_3: schema_conformance
  vertex_4: sanitization_complete
consensus: 4/4 (100%)
interpretation: "All 4 criteria define what valid input IS"
```

#### 2.1.2. SHOULD_LOCAL (Octahedron - Dual of Cube)
**Geometric Constraint**: Regular Octahedron  
**Schläfli Symbol**: {3,4}  
**Vertices**: 6  
**Consensus Required**: 5/6 (83.3%)  
**Duality**: Dual of Cube  
**Semantic**: Strong recommendation - what it SHOULD be  
**Context**: Private/Local

**Definition**: Strong supermajority of local criteria recommend a behavior. The octahedron is dual to the cube (MAY), representing the complementary relationship between recommendation and permission.

**Example**:
```yaml
requirement: "SHOULD_LOCAL cache computation results"
context: private
decision_vertices: [memory_available, cpu_benefit, cache_hit_rate, 
                    eviction_policy, coherency_simple, access_pattern]
consensus: 5/6 (83.3%)
interpretation: "5 of 6 criteria recommend caching"
```

#### 2.1.3. MAY_LOCAL (Cube - Dual of Octahedron)
**Geometric Constraint**: Regular Cube  
**Schläfli Symbol**: {4,3}  
**Vertices**: 8  
**Consensus Required**: 4/8 (50%)  
**Duality**: Dual of Octahedron  
**Semantic**: Optional permission - what it CAN be  
**Context**: Private/Local

**Definition**: Simple majority of local criteria permit a behavior. The cube is dual to the octahedron (SHOULD), representing the complementary relationship between permission and recommendation.

**Example**:
```yaml
requirement: "MAY_LOCAL use parallel processing"
context: private
decision_vertices: [cores_available, task_parallelizable, overhead_acceptable,
                    synchronization_simple, memory_sufficient, benefit_measurable,
                    complexity_manageable, debugging_feasible]
consensus: 4/8 (50%)
interpretation: "4 of 8 criteria permit parallelization"
```

#### 2.1.4. MUST_NOT_LOCAL (Dodecahedron - Dual of Icosahedron)
**Geometric Constraint**: Regular Dodecahedron  
**Schläfli Symbol**: {5,3}  
**Vertices**: 20  
**Consensus Required**: 18/20 (90%)  
**Duality**: Dual of Icosahedron  
**Semantic**: Strong prohibition - what it's NOT  
**Context**: Private/Local

**Definition**: Very strong supermajority prohibits a behavior. The dodecahedron is dual to the icosahedron (RECOMMENDED), representing the relationship between prohibition and alternative recommendation.

**Example**:
```yaml
requirement: "MUST_NOT_LOCAL expose internal pointers in public API"
context: private
decision_vertices: [20 security/encapsulation/maintenance criteria]
consensus: 18/20 (90%)
interpretation: "Overwhelming consensus prohibits pointer exposure"
```

#### 2.1.5. RECOMMENDED_LOCAL (Icosahedron - Dual of Dodecahedron)
**Geometric Constraint**: Regular Icosahedron  
**Schläfli Symbol**: {3,5}  
**Vertices**: 12  
**Consensus Required**: 10/12 (83.3%)  
**Duality**: Dual of Dodecahedron  
**Semantic**: Alternative recommendation  
**Context**: Private/Local

**Definition**: Strong supermajority recommends an alternative approach. The icosahedron is dual to the dodecahedron (MUST_NOT), suggesting alternatives to prohibited behaviors.

---

### 2.2. Protected/Intermediate Context (4-Polytopes)

Used for decisions crossing trust boundaries but within controlled federation.

#### 2.2.1. MUST_PROTECTED (5-cell - Self-Dual)
**Geometric Constraint**: Regular 5-cell (4-simplex)  
**Schläfli Symbol**: {3,3,3}  
**Vertices**: 5  
**Consensus Required**: 5/5 (100%)  
**Duality**: Self-dual  
**Semantic**: Positive assertion - what it IS in federation  
**Context**: Protected/Intermediate

**Definition**: All nodes in a federated cluster must unanimously agree. The 5-cell is the 4D analog of the tetrahedron and shares its self-dual property, representing unity between definition and requirement in federated context.

**Example**:
```yaml
requirement: "MUST_PROTECTED authenticate federation partner"
context: protected
decision_vertices:
  vertex_1: certificate_validity
  vertex_2: trust_anchor_verification
  vertex_3: revocation_check
  vertex_4: policy_compliance
  vertex_5: temporal_validity
consensus: 5/5 (100%)
interpretation: "All 5 federated criteria define what valid authentication IS"
```

#### 2.2.2. SHOULD_PROTECTED (24-cell - Self-Dual)
**Geometric Constraint**: Regular 24-cell  
**Schläfli Symbol**: {3,4,3}  
**Vertices**: 24  
**Consensus Required**: 20/24 (83.3%)  
**Duality**: Self-dual (unique to 4D)  
**Semantic**: Strong recommendation - what it SHOULD BE in federation  
**Context**: Protected/Intermediate

**Definition**: Strong supermajority across federated nodes recommend a behavior. The 24-cell is unique to 4D and self-dual, representing internal consistency across distributed nodes.

**Example**:
```yaml
requirement: "SHOULD_PROTECTED replicate state across cluster"
context: protected
decision_vertices: [24 cluster nodes]
consensus: 20/24 (83.3%)
rationale: "Tolerates up to 4 node failures while maintaining consensus"
```

#### 2.2.3. MAY_PROTECTED (8-cell - Dual of 16-cell)
**Geometric Constraint**: Regular 8-cell (hypercube/tesseract)  
**Schläfli Symbol**: {4,3,3}  
**Vertices**: 16  
**Consensus Required**: 8/16 (50%)  
**Duality**: Dual of 16-cell  
**Semantic**: Optional permission - what it CAN be in federation  
**Context**: Protected/Intermediate

**Definition**: Simple majority across federated nodes permit a behavior. The 8-cell is the 4D analog of the cube and dual to the 16-cell, representing the complementary relationship between permission and recommendation in federated context.

**Example**:
```yaml
requirement: "MAY_PROTECTED use eventual consistency"
context: protected
decision_vertices: [16 cluster nodes]
consensus: 8/16 (50%)
interpretation: "Simple majority permits eventual consistency"
```

#### 2.2.4. RECOMMENDED_PROTECTED (16-cell - Dual of 8-cell)
**Geometric Constraint**: Regular 16-cell (4-orthoplex)  
**Schläfli Symbol**: {3,3,4}  
**Vertices**: 8  
**Consensus Required**: 7/8 (87.5%)  
**Duality**: Dual of 8-cell  
**Semantic**: Very strong recommendation  
**Context**: Protected/Intermediate

**Definition**: Very strong supermajority recommends a behavior. The 16-cell is the 4D analog of the octahedron and dual to the 8-cell. Use for smaller federations needing higher consensus thresholds.

#### 2.2.5. MUST_NOT_PROTECTED (600-cell - Dual of 120-cell)
**Geometric Constraint**: Regular 600-cell  
**Schläfli Symbol**: {3,3,5}  
**Vertices**: 120  
**Consensus Required**: 108/120 (90%)  
**Duality**: Dual of 120-cell  
**Semantic**: Strong prohibition in large federation  
**Context**: Protected/Intermediate

**Definition**: Extremely strong prohibition across large federation. The 600-cell has the maximum vertices (120) among practical 4-polytopes, providing maximum scrutiny for prohibitions.

**Example**:
```yaml
requirement: "MUST_NOT_PROTECTED accept unauthenticated requests"
context: protected
decision_vertices: [120 security criteria across large federation]
consensus: 108/120 (90%)
```

---

### 2.3. Public/Shared Context (Archimedean Solids)

Used for decisions visible to external parties, public protocols, open standards.

#### 2.3.1. MUST_PUBLIC (Truncated Tetrahedron)
**Geometric Constraint**: Truncated Tetrahedron  
**Vertices**: 12  
**Consensus Required**: 12/12 (100%)  
**Catalan Dual**: Triakis Tetrahedron  
**Semantic**: Positive assertion - what the standard IS  
**Context**: Public/Shared

**Definition**: Unanimous agreement across public stakeholders. Truncation adds vertices while preserving tetrahedral symmetry. The Catalan dual represents compliance checkers viewing the standard from the inverse perspective.

**Example**:
```yaml
requirement: "MUST_PUBLIC support TLS 1.3 for public APIs"
context: public
decision_vertices: [security_requirement, industry_standard, 
                    client_compatibility, cipher_suite_support,
                    certificate_validation, protocol_version,
                    handshake_correctness, encryption_strength,
                    forward_secrecy, authentication_method,
                    key_exchange_algorithm, compliance_mandate]
consensus: 12/12 (100%)
interpretation: "All 12 stakeholder criteria define what secure API IS"
```

#### 2.3.2. SHOULD_PUBLIC (Cuboctahedron)
**Geometric Constraint**: Cuboctahedron  
**Vertices**: 12  
**Consensus Required**: 10/12 (83.3%)  
**Catalan Dual**: Rhombic Dodecahedron  
**Semantic**: Strong recommendation - what the standard SHOULD be  
**Context**: Public/Shared

**Definition**: Strong supermajority for public recommendations. Combines cubic and octahedral symmetry, representing consensus across diverse stakeholder types.

#### 2.3.3. MAY_PUBLIC (Truncated Cube)
**Geometric Constraint**: Truncated Cube  
**Vertices**: 24  
**Consensus Required**: 12/24 (50%)  
**Catalan Dual**: Triakis Octahedron  
**Semantic**: Optional - what the standard CAN be  
**Context**: Public/Shared

**Definition**: Simple majority for optional public features. Provides more granularity (24 vertices) than smaller Archimedean solids for complex stakeholder landscapes.

#### 2.3.4. RECOMMENDED_PUBLIC (Icosidodecahedron)
**Geometric Constraint**: Icosidodecahedron  
**Vertices**: 30  
**Consensus Required**: 25/30 (83.3%)  
**Catalan Dual**: Rhombic Triacontahedron  
**Semantic**: Strong recommendation for complex protocols  
**Context**: Public/Shared

**Definition**: Strong recommendation for complex public protocols. Combines icosahedral and dodecahedral symmetry, suitable for large stakeholder groups.

#### 2.3.5. MUST_NOT_PUBLIC (Truncated Icosidodecahedron)
**Geometric Constraint**: Truncated Icosidodecahedron  
**Vertices**: 120  
**Consensus Required**: 108/120 (90%)  
**Catalan Dual**: Disdyakis Triacontahedron  
**Semantic**: Very strong prohibition for public standards  
**Context**: Public/Shared

**Definition**: Very strong prohibition for public standards. Largest Archimedean solid provides maximum scrutiny across the most diverse stakeholder groups.

**Example**:
```yaml
requirement: "MUST_NOT_PUBLIC expose internal database schema"
context: public
decision_vertices: [120 security/privacy/compliance criteria]
consensus: 108/120 (90%)
rationale: "Overwhelming consensus prohibits schema exposure"
```

---

## 3. Context Selection Guidelines

### 3.1. Private/Local Context

**Use Platonic Solids When:**
- Decision involves 4-20 referenceable relations
- Decision remains within a single process/node
- No network communication required
- Internal state management
- Single-organization scope

**Examples:**
- Input validation (4 criteria)
- Local caching decisions (6-8 criteria)
- Memory management (8-12 criteria)
- Thread synchronization (4-12 criteria)
- Internal data structures (8-20 criteria)

### 3.2. Protected/Intermediate Context

**Use 4-Polytopes When:**
- Decision involves 5-120 referenceable relations across nodes
- Decision crosses trust boundaries within federation
- Coordination between known, authenticated nodes
- Cluster consensus mechanisms
- Inter-service communication within organization

**Examples:**
- Small cluster consensus (5 nodes → 5-cell)
- Medium cluster (8 nodes → 16-cell, 16 nodes → 8-cell)
- Large cluster (24 nodes → 24-cell)
- Global federation (120 nodes → 600-cell)
- Cross-datacenter replication

### 3.3. Public/Shared Context

**Use Archimedean Solids When:**
- Decision involves 12-120 referenceable relations across organizations
- Decision affects external parties
- Public API or protocol specification
- Multi-organization coordination
- External visibility required
- Need flexibility in matching exact stakeholder counts (13 polyhedra provide options)

**Examples:**
- Small API standard (12 stakeholders → Truncated Tetrahedron)
- Medium protocol (24 stakeholders → Truncated Cube)
- Large standard (60 stakeholders → Truncated Dodecahedron)
- Global standard (120 stakeholders → Truncated Icosidodecahedron)

---

## 4. Bipartite Algebraic Proof System

### 4.1. Three-Partition Proof Structure

The proof system uses three partitions:

**Left Partition (Geometric Constraints by Context):**
```
Private:    {Tetrahedron, Cube, Octahedron, Dodecahedron, Icosahedron}
Protected:  {5-cell, 8-cell, 16-cell, 24-cell, 600-cell}
Public:     {Truncated Tetrahedron, Cuboctahedron, Truncated Cube, ...}
```

**Middle Partition (Algebraic Laws):**
```
Unanimity:              |A| = |V|        (100%)
Strong Supermajority:   |A| ≥ 0.833|V|  (83.3%)
Very Strong Supermajority: |A| ≥ 0.875|V| (87.5%)
Simple Majority:        |A| ≥ 0.5|V|    (50%)
Near-Unanimity:         |A| ≥ 0.9|V|    (90%)
```

**Right Partition (Concrete Decisions with Context):**
```
{requirement, context, vertices, consensus}
```

### 4.2. Proof Certificate Format

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

### 4.3. Verification Algorithm

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

---

## 5. Formal Semantics with Context and Duality

### 5.1. Context Function

Define a context function that maps requirements to visibility contexts:

```
context: Requirement → {private, protected, public}
```

### 5.2. Geometric Constraint Function (Context-Aware)

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

### 5.3. Duality Semantic Function

```
duality_semantic: Polyhedron → SemanticMeaning

Self-dual polyhedra:
  duality_semantic(Tetrahedron) = "Positive assertion - what it IS"
  duality_semantic(5-cell) = "Positive assertion - what it IS in federation"
  duality_semantic(24-cell) = "Strong recommendation - what it SHOULD BE"

Dual pairs (permission ↔ recommendation):
  duality_semantic(Cube) = "Permission - what it CAN be"
  duality_semantic(Octahedron) = "Recommendation - what it SHOULD be"
  duality_semantic(8-cell) = "Permission - what it CAN be in federation"
  duality_semantic(16-cell) = "Recommendation - what it SHOULD be in federation"

Inverse duality (prohibition ↔ alternative):
  duality_semantic(Dodecahedron) = "Prohibition - what it's NOT"
  duality_semantic(Icosahedron) = "Alternative - what's SUGGESTED instead"
```

### 5.4. Context-Aware Validity Predicate

A requirement R with normative keyword K in context C is valid if and only if:

```
valid(R, K, C, V) ⟺ 
  context(R) = C ∧
  |V| = vertex_count(geometric_constraint(K, C)) ∧
  |A| ≥ threshold(geometric_constraint(K, C)) ∧
  context_appropriate(R, C) ∧
  duality_semantics_preserved(K, geometric_constraint(K, C))
```

Where:
- `context_appropriate` verifies visibility scope matches actual usage
- `duality_semantics_preserved` verifies semantic meaning aligns with geometric duality

---

## 6. Rationale for Geometric Choices

### 6.1. Why Not Just Use Vertex Counts Directly?

**Question**: Why not simply specify "12 stakeholders must agree" instead of "Truncated Tetrahedron"?

**Answer**: 
1. **Mathematical Structure**: Polyhedra provide built-in validation—vertex counts, edge counts, and face counts have fixed relationships (Euler's formula). This catches specification errors.
2. **Duality Semantics**: Geometric duality encodes meaningful relationships (permission ↔ recommendation, prohibition ↔ alternative) that pure counts lack.
3. **Consensus Patterns**: Regular polyhedra represent stable, symmetric consensus patterns proven mathematically sound.
4. **Verification Simplicity**: Algebraic laws on polyhedra enable O(1) verification vs O(n³) graph traversal.

### 6.2. Why Three Geometric Families Instead of One?

**Question**: Why not use only Platonic solids for all contexts?

**Answer**: Three families address different scales and coordination complexities:

1. **Platonic Solids (3D)**: Perfect for bounded local decisions (4-20 criteria) where all criteria are equally weighted and coordination is trivial.

2. **Regular 4-Polytopes (4D)**: Essential for federated decisions (5-120 nodes) where higher-dimensional symmetry captures cross-boundary coordination complexity. The 4D polytopes provide vertex counts (5, 8, 16, 24, 120) that naturally fit cluster sizes while maintaining regular structure.

3. **Archimedean Solids (3D)**: Necessary for public standards (12-120 stakeholders) where semi-regular structure accommodates diverse stakeholder types while maintaining vertex-transitive fairness.

**Mathematical Necessity**: No single geometric family spans the full 4-120 vertex range while maintaining appropriate symmetry properties for each context's coordination requirements.

### 6.3. Why Duality Matters for Semantic Meaning

**Question**: Why encode semantics in geometric duality rather than just percentages?

**Answer**: Duality provides:
1. **Semantic Coherence**: Self-dual polyhedra naturally represent positive assertions (what something IS) because the structure looks the same from dual perspectives.
2. **Complementary Relationships**: Cube↔Octahedron duality perfectly encodes the permission↔recommendation relationship—what you CAN do vs what you SHOULD do.
3. **Inverse Perspectives**: Dodecahedron↔Icosahedron duality captures the prohibition↔alternative relationship—what you MUST NOT do vs what's RECOMMENDED instead.
4. **Mathematical Guarantees**: Duality ensures semantic relationships are mathematically grounded, not arbitrarily assigned.

---

## 7. Implementation Considerations

### 7.1. Tooling Support

**Geometric Constraint Libraries**:
```python
class GeometricConstraint:
    def __init__(self, family: str, name: str, vertices: int, 
                 threshold: float, duality: str, semantic: str):
        self.family = family  # "platonic", "4-polytope", "archimedean"
        self.name = name
        self.vertices = vertices
        self.threshold = threshold
        self.duality = duality
        self.semantic = semantic

# Pre-defined constraints for each context
PRIVATE_CONSTRAINTS = {
    "MUST_LOCAL": GeometricConstraint("platonic", "Tetrahedron", 4, 1.0, "self-dual", "IS"),
    "SHOULD_LOCAL": GeometricConstraint("platonic", "Octahedron", 6, 0.833, "dual_of_cube", "SHOULD_BE"),
    # ... etc
}
```

**Proof Certificate Generator**:
```python
def generate_proof(requirement: str, context: str, vertices: List[Vertex]) -> Certificate:
    constraint = select_constraint(requirement.normative_keyword, context)
    agrees_count = count_agreements(vertices)
    valid = agrees_count >= constraint.threshold * constraint.vertices
    
    return Certificate(
        requirement=requirement,
        context=context,
        constraint=constraint,
        agrees_count=agrees_count,
        valid=valid,
        proof=f"Algebraic: {agrees_count}/{constraint.vertices} >= {constraint.threshold}",
        context_justification=justify_context(requirement, context)
    )
```

### 7.2. Migration from RFC 2119

**Backward Compatibility**:
- RFC 2119 keywords remain valid but context-agnostic
- New contextual keywords (MUST_LOCAL, MUST_PROTECTED, MUST_PUBLIC) provide explicit consensus requirements
- Tools can interpret plain RFC 2119 keywords using default context mappings

**Default Context Mappings** (for backward compatibility):
```
RFC 2119 Keyword → Default Geometric Constraint
MUST           → MUST_PUBLIC (Truncated Tetrahedron, 12/12)
SHOULD         → SHOULD_PUBLIC (Cuboctahedron, 10/12)  
MAY            → MAY_PUBLIC (Truncated Cube, 12/24)
MUST_NOT       → MUST_NOT_PUBLIC (Truncated Icosidodecahedron, 108/120)
RECOMMENDED    → RECOMMENDED_PUBLIC (Icosidodecahedron, 25/30)
```

### 7.3. Performance Characteristics

**Verification Complexity**:
- Context identification: O(1)
- Constraint selection: O(1) via lookup table
- Agreement counting: O(v) where v = vertex count
- Context appropriateness: O(1) with proper scoping
- Total: O(v) - linear in decision complexity

**Certificate Size**:
- Fixed portion: ~1KB (constraint metadata, proof structure)
- Variable portion: O(v) for vertex list
- Typical: 1-5KB for most practical decisions

---

## 8. Security Considerations

### 8.1. Context Confusion Attacks

**Risk**: Malicious actors may attempt to apply private-context decisions to public contexts or vice versa, exploiting different consensus thresholds.

**Mitigation**:
- Explicit context tagging in all specifications
- Automated context verification during requirement validation
- Clear scope boundaries in implementation

### 8.2. Vertex Manipulation Attacks

**Risk**: Attackers may attempt to add/remove decision vertices to manipulate consensus outcomes.

**Mitigation**:
- Immutable vertex lists in proof certificates
- Cryptographic signing of vertex definitions
- Fixed vertex counts based on geometric constraints

### 8.3. Proof Forgery

**Risk**: Fabricated proof certificates claiming requirement satisfaction.

**Mitigation**:
- Digital signatures on all certificates
- Certificate transparency logs
- Cross-verification of geometric constraints

### 8.4. Context Appropriateness Verification

**Implementation MUST** verify that the selected context matches the actual visibility scope of the decision. Private-context requirements MUST NOT be used for public-facing APIs without explicit re-evaluation.

---

## 9. IANA Considerations

This document proposes registration of the following new normative keyword families:

### 9.1. Private Context Keywords
- MUST_LOCAL
- SHOULD_LOCAL  
- MAY_LOCAL
- MUST_NOT_LOCAL
- RECOMMENDED_LOCAL

### 9.2. Protected Context Keywords
- MUST_PROTECTED
- SHOULD_PROTECTED
- MAY_PROTECTED
- MUST_NOT_PROTECTED
- RECOMMENDED_PROTECTED

### 9.3. Public Context Keywords
- MUST_PUBLIC
- SHOULD_PUBLIC
- MAY_PUBLIC
- MUST_NOT_PUBLIC
- RECOMMENDED_PUBLIC

### 9.4. Geometric Constraint Registry

This document also establishes a registry for geometric constraints used in normative specifications, including:
- Polyhedron/polytope names and properties
- Vertex counts and consensus thresholds
- Duality relationships
- Context applicability

---

## 10. References

### 10.1. Normative References
- [RFC2119] Bradner, S., "Key words for use in RFCs to Indicate Requirement Levels", BCP 14, RFC 2119, March 1997.

### 10.2. Informative References
- Coxeter, H. S. M., "Regular Polytopes", Dover Publications, 1973.
- Ziegler, G. M., "Lectures on Polytopes", Springer, 1995.
- Grünbaum, B., "Convex Polytopes", Springer, 2003.

---

## Appendix A. Complete Geometric Reference

### A.1. Platonic Solids (Private Context)

| Solid | Schläfli | Vertices | Faces | Edges | Dual | Consensus |
|-------|----------|----------|-------|-------|------|-----------|
| Tetrahedron | {3,3} | 4 | 4 | 6 | Self | 4/4 (100%) |
| Cube | {4,3} | 8 | 6 | 12 | Octahedron | 4/8 (50%) |
| Octahedron | {3,4} | 6 | 8 | 12 | Cube | 5/6 (83.3%) |
| Dodecahedron | {5,3} | 20 | 12 | 30 | Icosahedron | 18/20 (90%) |
| Icosahedron | {3,5} | 12 | 20 | 30 | Dodecahedron | 10/12 (83.3%) |

### A.2. Regular 4-Polytopes (Protected Context)

| Polytope | Schläfli | Vertices | Edges | Faces | Cells | Dual | Consensus |
|----------|-----------|----------|-------|-------|-------|------|-----------|
| 5-cell | {3,3,3} | 5 | 10 | 10 | 5 | Self | 5/5 (100%) |
| 8-cell | {4,3,3} | 16 | 32 | 24 | 8 | 16-cell | 8/16 (50%) |
| 16-cell | {3,3,4} | 8 | 24 | 32 | 16 | 8-cell | 7/8 (87.5%) |
| 24-cell | {3,4,3} | 24 | 96 | 96 | 24 | Self | 20/24 (83.3%) |
| 600-cell | {3,3,5} | 120 | 720 | 1200 | 600 | 120-cell | 108/120 (90%) |

### A.3. Archimedean Solids (Public Context)

| Solid | Vertices | Faces | Edges | Catalan Dual | Consensus |
|-------|----------|-------|-------|--------------|-----------|
| Truncated Tetrahedron | 12 | 8 | 18 | Triakis Tetrahedron | 12/12 (100%) |
| Cuboctahedron | 12 | 14 | 24 | Rhombic Dodecahedron | 10/12 (83.3%) |
| Truncated Cube | 24 | 14 | 36 | Triakis Octahedron | 12/24 (50%) |
| Truncated Octahedron | 24 | 14 | 36 | Tetrakis Hexahedron | 12/24 (50%) |
| Rhombicuboctahedron | 24 | 26 | 48 | Deltoidal Icositetrahedron | 12/24 (50%) |
| Truncated Cuboctahedron | 48 | 26 | 72 | Disdyakis Dodecahedron | 24/48 (50%) |
| Snub Cube | 24 | 38 | 60 | Pentagonal Icositetrahedron | 20/24 (83.3%) |
| Icosidodecahedron | 30 | 32 | 60 | Rhombic Triacontahedron | 25/30 (83.3%) |
| Truncated Dodecahedron | 60 | 32 | 90 | Triakis Icosahedron | 54/60 (90%) |
| Truncated Icosahedron | 60 | 32 | 90 | Pentakis Dodecahedron | 54/60 (90%) |
| Rhombicosidodecahedron | 60 | 62 | 120 | Deltoidal Hexecontahedron | 50/60 (83.3%) |
| Truncated Icosidodecahedron | 120 | 62 | 180 | Disdyakis Triacontahedron | 108/120 (90%) |
| Snub Dodecahedron | 60 | 92 | 150 | Pentagonal Hexecontahedron | 50/60 (83.3%) |

---

## Appendix B. Examples of Contextual Requirements

### B.1. Private Context Example: Input Validation

```yaml
specification: "Local data processing API"
requirement: "MUST_LOCAL validate JSON schema before parsing"
context: private
geometric_constraint: 
  name: "Tetrahedron"
  vertices: 4
  threshold: 4/4 (100%)
decision_vertices:
  - id: "syntax"
    description: "JSON syntax validity"
    agrees: true
  - id: "structure" 
    description: "Object structure matches schema"
    agrees: true
  - id: "types"
    description: "Data types conform to specification"
    agrees: true
  - id: "constraints"
    description: "Value constraints satisfied"
    agrees: true
consensus: 4/4 (100%)
valid: true
proof_certificate: "0x8a3f..."
```

### B.2. Protected Context Example: Cluster Authentication

```yaml  
specification: "Federated identity system"
requirement: "MUST_PROTECTED verify certificate chain for cluster nodes"
context: protected
geometric_constraint:
  name: "5-cell"
  vertices: 5
  threshold: 5/5 (100%)
decision_vertices:
  - node: "auth-server-1"
    agrees: true
  - node: "auth-server-2" 
    agrees: true
  - node: "auth-server-3"
    agrees: true
  - node: "auth-server-4"
    agrees: true
  - node: "auth-server-5"
    agrees: true
consensus: 5/5 (100%)
valid: true
```

### B.3. Public Context Example: Web API Standard

```yaml
specification: "RESTful API Design Standard"
requirement: "MUST_PUBLIC use HTTPS for all endpoints"
context: public  
geometric_constraint:
  name: "Truncated Tetrahedron"
  vertices: 12
  threshold: 12/12 (100%)
decision_vertices:
  - stakeholder: "security"
    agrees: true
  - stakeholder: "privacy"
    agrees: true
  - stakeholder: "compliance"
    agrees: true
  - stakeholder: "browser-vendors"
    agrees: true
  - stakeholder: "mobile-platforms"
    agrees: true
  - stakeholder: "api-consumers"
    agrees: true
  - stakeholder: "api-providers"
    agrees: true
  - stakeholder: "cdn-providers"
    agrees: true
  - stakeholder: "standards-body"
    agrees: true
  - stakeholder: "regulatory"
    agrees: true  
  - stakeholder: "accessibility"
    agrees: true
  - stakeholder: "performance"
    agrees: true
consensus: 12/12 (100%)
valid: true
```

---

## Appendix C. Formal Verification Pseudocode

```
// Context-aware requirement verification
function verifyRequirement(requirement, context, vertices) {
    // 1. Context validation
    if (!isContextAppropriate(requirement, context)) {
        return {valid: false, error: "Context mismatch"};
    }
    
    // 2. Constraint selection
    constraint = selectConstraint(requirement.normativeKeyword, context);
    
    // 3. Vertex count validation
    if (vertices.length != constraint.vertices) {
        return {valid: false, error: `Expected ${constraint.vertices} vertices, got ${vertices.length}`};
    }
    
    // 4. Agreement counting
    agrees = vertices.filter(v => v.agrees).length;
    threshold = Math.ceil(constraint.vertices * constraint.threshold);
    
    // 5. Duality semantics verification
    if (!verifyDualitySemantics(requirement, constraint)) {
        return {valid: false, error: "Duality semantics violation"};
    }
    
    valid = agrees >= threshold;
    
    // 6. Generate proof certificate
    certificate = {
        requirement: requirement,
        context: context,
        constraint: constraint,
        agrees: agrees,
        required: threshold,
        valid: valid,
        proof: generateAlgebraicProof(constraint, agrees, threshold),
        timestamp: now(),
        signature: sign(requirement + context + agrees)
    };
    
    return {valid: valid, certificate: certificate};
}

// Context appropriateness verification
function isContextAppropriate(requirement, context) {
    visibility = analyzeRequirementVisibility(requirement);
    
    switch(context) {
        case "private":
            return visibility.scope == "internal" && 
                   visibility.stakeholders <= 20;
        case "protected": 
            return visibility.scope == "federated" &&
                   visibility.stakeholders >= 5 &&
                   visibility.stakeholders <= 120;
        case "public":
            return visibility.scope == "external" &&
                   visibility.stakeholders >= 12;
        default:
            return false;
    }
}
```

---

**Authors' Addresses**:

*Geometric Standards Working Group*  
*Internet Engineering Task Force*  
*Email: geometric-standards@ietf.org*

---

**Acknowledgments**:

The authors thank the mathematical physics community for foundational work on polyhedral symmetries, and the distributed systems community for practical insights into consensus mechanisms across different coordination scopes.
