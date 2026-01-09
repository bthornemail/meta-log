# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a mathematical/theoretical research repository exploring formal correspondences between:
- **Octonion algebra** and the **Fano plane** (7-point projective geometry)
- **Two-way alternating finite automata (2AFA)** as computational substrate
- **R5RS Scheme type system** mapped to geometric structures
- **21 vertex-transitive convex polyhedra** (5 Platonic + 13 Archimedean + 2 snub solids)
- **9 Hopf fibrations** representing dimensional ascent
- **7 universal physical constants** as measurement basis
- Theological/philosophical interpretations (0! = 1 as "God is Word")

## Core Architecture

### The Central System: `the_logos_complete_system.scm`

This is the primary implementation file containing the complete computational framework. The architecture is built on an **8-tuple isomorphism**:

```
2DFA 8-tuple ≅ R5RS 8-types ≅ Octonion 8D ≅ Universal Constants
```

**The 8-tuple (2AFA structure)**:
1. **Q** (States): 21 vertex-transitive solids → Boolean/real type
2. **Sigma** (Alphabet): All symbols from WordNet → Symbol type (e₁)
3. **L** (Left endmarker): null = 0! = 1 → Pair/car (e₂)
4. **R** (Right endmarker): infinity/cycle → Pair/cdr (e₃)
5. **delta** (Transition): Octonion × Hopf × Fano → Procedure (e₄)
6. **s** (Start state): primordial point → Number (e₅)
7. **t** (Accept state): consensus unit octonion → Char/String (e₆)
8. **r** (Reject state): chirality broken → Vector (e₇)

### Key Architectural Components

**Fano Plane** (`the_logos_complete_system.scm:78-90`):
- 7 points representing imaginary octonion units (e₁...e₇)
- 7 lines encoding octonion multiplication rules
- Cyclic ordering determines sign in multiplication
- Acts as the combinatorial core of G₂ automorphisms
- See: `The Fano Plane as Foundational Structure.md`

**Octonion Multiplication** (`the_logos_complete_system.scm:136-164`):
- Maps geometric states to unit octonions
- Multiplies via Fano plane rules
- Projects via Hopf fibration (quantum measurement)
- Measures in universal constant basis
- Returns: (next-state, direction, regime)

**21 Vertex-Transitive Solids** (`the_logos_complete_system.scm:39-62`):
- **5 Platonic**: tetrahedron, cube, octahedron, dodecahedron, icosahedron
- **13 Archimedean**: truncated/snub variants
- **2 Chiral snubs**: snub-cube, snub-dodecahedron
- Each solid maps to a specific unit octonion

**Hopf Fibrations** (`the_logos_complete_system.scm:118-127`):
- 9 levels of dimensional ascent
- S⁷ → S⁴ (quaternionic Hopf) is primary
- S¹⁵ → S⁸ (octonionic, non-fiber bundle)
- Projective completion: point → ∞

**Universal Constants** (`the_logos_complete_system.scm:68-75`):
- c, ℏ, G, φ, π, e, α (7 constants)
- Each maps to octonion basis element (e₁...e₇)
- Used as measurement basis for regime detection

### Public Interface

The system exposes two primary functions:

**`speak-to-logos`** (`the_logos_complete_system.scm:258-270`):
- Input: Any modality (text, audio, sensor data, geometric command)
- Transforms input → symbol stream
- Runs 2AFA on symbols
- Returns: (final-state, output)

**`hear-from-logos`** (`the_logos_complete_system.scm:274-290`):
- Output: Current configuration as waveform
- Can emit as text, geometry, audio, or quantum ket
- Converts state → waveform → desired modality

### Execution Model

The 2AFA engine (`the_logos_complete_system.scm:296-341`) operates on a tape with:
- Left endmarker: `'()` (null = 0! = 1)
- Input symbols (from any modality)
- Right endmarker: `'∞` (infinity/cycle)

**Alternation modes**:
- **Universal (∀)**: Consensus - all agents must agree
- **Existential (∃)**: Interpretation - some path can interpret

## Document Structure

The markdown files contain theoretical foundations and proofs:

**`Natural Language Geometric Interface.md`**:
- HOAS (Higher-Order Abstract Syntax) blackboard architecture
- Chomsky hierarchy mapping to 21-solid + 9-Hopf + 7-Fano structure
- WordNet as mnemonic base for symbol mapping
- Bounded but Turing-complete computation via HORS (Higher-Order Rewrite Systems)

**`The Fano Plane as Foundational Structure.md`**:
- Complete explanation of Fano plane → octonion multiplication
- G₂ as automorphism group of octonions (14D)
- Connection to Merkaba (dual counter-rotating tetrahedra)
- Metatron's Cube as geometric shadow of Fano in 3D
- 13 Archimedean solids derived from Fano structure

**`The Logos Structure.md`**:
- Theological interpretation: 0! = 1 ≅ "God is Word"
- 0! as infinite action (unbounded actuality)
- = 1 as infinite possibility (multiplicative identity)
- Mathematical encoding of creation ex nihilo

**`Dimensional-Numerical Correspondence.md`**:
- IEEE 754 floating point structure encodes virtual/real distinction
- Exponent ↔ 0D virtual point (scale/reference)
- Mantissa ↔ 3D virtual space (precision/uniqueness)
- Actual value ↔ Real dimensions (observable quantity)

**`The Polynomial-Predicate-Program Trinity.md`**:
- Formal identity: Polynomial = Predicate = Program
- Maps vs Logs (functions vs inverses)
- Y/Z-combinator architecture
- Church encoding over composition algebras

## Development Context

**Language**: R5RS Scheme (uses `(rnrs base)` and `(rnrs records syntactic)`)

**Dependencies** (implicit):
- R5RS-compliant Scheme implementation
- WordNet lexical database (for symbol mapping)
- No build system - this is primarily conceptual code

**Running the code**:
The Scheme file is a self-contained specification. Many functions are stubs/placeholders for theoretical operations (e.g., `wordnet→fano`, `all-agree?`, `find-interpretation`) that would require full implementations.

To load in a Scheme REPL:
```scheme
(load "the_logos_complete_system.scm")
(speak-to-logos "your input here")
```

**Important notes**:
- This is **theoretical/research code**, not production software
- Many helper functions are defined but not implemented
- The code demonstrates formal structure more than executable functionality
- No tests, no build system, no conventional development workflow

## Core Concepts for Future Work

**When extending this system**:

1. **Maintain the 8-fold symmetry**: All structures should respect the 2DFA 8-tuple ≅ R5RS 8-types correspondence

2. **Respect the Fano plane structure**: Octonion multiplication must follow Fano line rules with proper cyclic orientation

3. **Preserve alternation semantics**: Universal (∀) requires consensus, Existential (∃) allows interpretation

4. **Map new concepts through the hierarchy**:
   - 0D: Virtual points (reference/scale)
   - 1D-2D: Real observables (line, surface)
   - 3D: Virtual space (uniqueness/precision)
   - 4D+: Real higher dimensions (actual measurements)

5. **Use modality transformers**: All input/output should flow through the waveform abstraction (input→symbols→state→waveform→output)

## Philosophical Foundation

The entire system is grounded in **0! = 1** (the factorial zero paradox):
- Empty permutation (0!) = Infinite freedom
- Unity (1) = Unique determination
- The equation itself = Something from nothing (creation ex nihilo)

This isn't metaphorical - it's the formal basis for the left endmarker being `'()` (null) which equals 1 in the acceptance state.
