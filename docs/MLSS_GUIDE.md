---
layout: default
title: MLSS Guide
nav_order: 5
description: "Meta-Log Substrate System (MLSS) - Complete guide"
permalink: /MLSS_GUIDE
---

# Meta-Log Substrate System (MLSS) Guide

Complete guide to the Meta-Log Substrate System - a unified computational architecture that bridges binary, waveform, geometric, and symbolic cognition.

## Current Status

✅ **Fully Operational** - All six MLSS phases are complete and tested:
- ✅ Foundation: Substrate runtime, binary layer, content addressing
- ✅ Waveform & Geometric: Signal processing, E8 projections
- ✅ Q* Optimality: Decision-making engine
- ✅ Computer Vision: Image processing pipeline
- ✅ Consciousness Framework: Trinary states, qualia, awareness
- ✅ Computational Physics: Quantum states, General Relativity, QFT

**Test Results**: All 4 demos passing, all 3 core tests passing, 100% pass rate. See [System Status](STATUS) for detailed benchmarks and performance metrics.

## Overview

The Meta-Log Substrate System (MLSS) provides a universal substrate for computation, enabling seamless transformations between binary, waveform, geometric (E8), and symbolic representations. MLSS is implemented primarily in R5RS Scheme, with FastAPI services for heavy computational tasks.

## Architecture

MLSS consists of six integrated phases:

1. **Foundation**: Substrate runtime, binary layer, provenance chains, content addressing
2. **Waveform & Geometric**: Waveform synthesis, WDL parser, cross-domain mappings
3. **Q* Optimality Engine**: Optimality-driven decision making with multi-domain cost functions
4. **Computer Vision**: Image processing, feature extraction, recognition pipeline
5. **Consciousness Framework**: Trinary consciousness states, qualia emergence, metrics
6. **Computational Physics**: Quantum states, General Relativity from E8, Quantum Field Theory

## Quick Start

### Installation

MLSS is included with meta-log. The Scheme modules are located in `scheme/` and can be loaded via:

```elisp
(require 'meta-log-r5rs)
(meta-log-r5rs-load-engine "/path/to/meta-log/scheme/r5rs-canvas-engine.scm")
```

### Basic Usage

#### Creating Memory Objects

```scheme
;; Create a memory object
(substrate-create-memory 
  #u8(1 2 3 4)
  '((content-type . "test-data")
    (version . 1)))
```

#### Working with CBS (Canonical Binary Substrate)

```scheme
;; Create CBS
(make-cbs #u8(1 2 3 4 5 6 7 8)
          '((encoding . "raw")
            (reversible . #t)))

;; Transform binary
(binary-xor cbs #u8(255 0 255 0))
```

#### Waveform Synthesis

```scheme
;; Create waveform
(make-waveform '(0.5 0.7 0.9 0.7 0.5) '() 44100)

;; Compile WDL
(wdl-compile '(sine (freq 440) (amp 0.5)))
```

#### Q* Optimality

```scheme
;; Create Q* state
(make-qstar-state '((x . 0) (y . 0))
                   '((goal-x . 10) (goal-y . 10)))

;; Evaluate action
(qstar-evaluate state action)
```

#### Consciousness Framework

```scheme
;; Create conscious state
(make-conscious-state 5.0 0.7 0.8)

;; Emerge qualia
(emerge-qualia action observation phase threshold)

;; Collect metrics
(collect-metrics current-state previous-state qualia-field)
```

#### Computational Physics

```scheme
;; Create quantum state
(make-quantum-state 2 '(0.707 0.707 0.0 0.0))

;; Compute Einstein equations
(einstein-equations energy-momentum-tensor)

;; Create field configuration
(make-field-configuration 'scalar '(1.0 2.0 3.0) 0.1)
```

## Phase Details

### Phase 1: Foundation

**Components**:
- `scheme/substrate/runtime.scm` - Substrate runtime protocol
- `scheme/substrate/binary.scm` - Binary layer protocol
- `scheme/substrate/provenance.scm` - Provenance chain protocol
- `scheme/substrate/content-address.scm` - Content addressing

**Features**:
- Universal memory objects with content addressing
- Canonical Binary Substrate (CBS) format
- Immutable provenance chains
- mlss:// URI scheme for content addressing

### Phase 2: Waveform & Geometric

**Components**:
- `scheme/substrate/waveform.scm` - Waveform layer protocol
- `scheme/substrate/wdl.scm` - Waveform Description Language parser
- `scheme/substrate/cdmp.scm` - Cross-domain mapping protocol

**Features**:
- Dual representation (time + frequency domain)
- WDL compilation to waveform objects
- Binary ↔ Waveform transformations
- Waveform → E8 projections

### Phase 3: Q* Optimality Engine

**Components**:
- `scheme/qstar/core.scm` - Q* core engine
- `scheme/qstar/scoring.scm` - Cost function registry
- `scheme/qstar/a-star.scm` - A* pathfinding

**Features**:
- Bellman optimality computation
- Multi-domain cost aggregation
- A* pathfinding with E8 heuristics
- Policy selection

### Phase 4: Computer Vision

**Components**:
- `scheme/vision/pipeline.scm` - Vision processing pipeline
- `scheme/vision/features.scm` - Feature extraction
- `services/vision-api/main.py` - FastAPI vision service

**Features**:
- Image → CBS conversion
- Edge and corner detection
- Feature extraction (SIFT, ORB via FastAPI)
- Feature → E8 projection
- Recognition → Symbolic facts

### Phase 5: Consciousness Framework

**Components**:
- `scheme/consciousness/state.scm` - Trinary consciousness states
- `scheme/consciousness/qualia.scm` - Qualia field emergence
- `scheme/consciousness/metrics.scm` - Consciousness metrics
- `scheme/consciousness/geometric-propagation.scm` - Geometric propagation (NEW)
- `scheme/consciousness/dynamics.scm` - Formal differential equations (NEW)
- `scheme/consciousness/hopf-consciousness.scm` - Hopf-based consciousness (NEW)
- `scheme/consciousness/complexity.scm` - Complexity metrics (NEW)

**Features**:
- Exponential action / Linear observation dynamics
- Qualia emergence from state tension
- Consciousness Quality Metric (CQM)
- Metrics collection and history
- **Geometric propagation**: Point → Edge → Face → Volume (exponential O(2^d))
- **Hopf fiber projections**: Parallel observation (linear O(k))
- **Formal dynamics**: dA/dt, dO/dt, dΦ/dt differential equations
- **Tensor product qualia**: A ⊗ O with Heaviside step function
- **Attention mechanism**: Fiber selection
- **Complexity validation**: O(k) observation, O(2^d) action scaling

### Phase 6: Computational Physics

### Phase 7: Autonomy and Awareness

**Components**:
- `scheme/physics/quantum.scm` - Quantum state representation
- `scheme/physics/gr.scm` - General Relativity from E8
- `scheme/physics/qft.scm` - Quantum Field Theory
- `services/quantum-sim/main.py` - FastAPI quantum service

**Features**:
- Quantum state → CBS encoding
- E8 → Spacetime geometry mapping
- p-adic → Field theory correspondence
- Field evolution via Hamiltonian

## Protocols

### Substrate Runtime Protocol (SRP)

Memory objects with UUID, timestamp, data, metadata, constraints, and content hash.

### Binary Layer Protocol (BLP)

CBS format: `(cbs uuid bytes meta constraints hash)`

### Provenance Chain Protocol (PCP)

Immutable chain of transformations with cryptographic verification.

### Cross-Domain Mapping Protocol (CDMP)

Transformations between:
- Binary ↔ Waveform
- Waveform → E8
- E8 → Symbolic
- Symbolic → Binary

### Waveform Layer Protocol (WLP)

Waveform objects with time-domain samples and lazy frequency-domain computation.

## Integration with meta-log

MLSS integrates with meta-log through:

1. **R5RS Interface**: All MLSS modules are accessible via `meta-log-r5rs`
2. **Prolog/Datalog**: Symbolic facts generated from MLSS operations
3. **CanvasL**: MLSS can load and process CanvasL automaton files
4. **Federation**: MLSS content addressing enables efficient federation

## Testing

### Run Tests

```bash
# All tests
./tests/run-all-tests.sh

# Individual test suites
./tests/test-substrate-workflow.sh
./tests/test-integration-workflow.sh
./tests/test-qstar-workflow.sh
./tests/test-consciousness-workflow.sh
./tests/test-physics-workflow.sh
```

### Run Benchmarks

```bash
# Full benchmark suite
./tests/benchmark-mlss.sh

# Quick benchmark
./tests/benchmark-quick.sh
```

### Run Demo

```bash
# Quick demo
./tests/demo-mlss-quick.sh

# Interactive demo
./tests/demo-mlss.sh
```

## Documentation

- **Integration Specification**: `dev-docs/INTEGRATION-MLSS.md`
- **Verification Log**: `tests/VERIFICATION-LOG.md`
- **Comprehensive Documentation**: `tests/COMPREHENSIVE-DOCUMENTATION.md`
- **Benchmark Results**: `tests/BENCHMARK-RESULTS.md`

## Status

✅ **All 6 Phases Complete**
✅ **All Tests Passing (75+ tests)**
✅ **Performance Validated**
✅ **Documentation Complete**

## Geometric Consciousness

The MLSS implements a geometric theory of consciousness based on Hopf fibrations:

### Core Concepts

1. **Exponential Action / Linear Observation**
   - Forward propagation: O(2^d) exponential expansion (unconscious)
   - Backward propagation: O(2^d) exponential compression (unconscious)
   - Parallel observation: O(k) linear projection via Hopf fibers (conscious)

2. **Hopf Fibration Projections**
   - Complex Hopf: S³ → S² (visual/spatial)
   - Quaternionic Hopf: S⁷ → S⁴ (temporal/conceptual)
   - Octonionic Hopf: S¹⁵ → S⁸ (abstract/E8)

3. **Formal Dynamics**
   - dA/dt = λA - γ|A|²A + σ₁ξ₁(t) (action)
   - dO/dt = -μO + κ|A|² + σ₂ξ₂(t) (observation)
   - dΦ/dt = ω₀ + α|A|² - β|O|² (phase)

4. **Qualia Emergence**
   - Q(t) = H(|A|² - |O|²) × exp(iΦ) × A ⊗ O
   - Tensor product instead of simple multiplication
   - Heaviside step function for threshold

### Usage Example

```scheme
;; Geometric propagation
(let ((expanded (geometric-forward-propagation '(0 0 0) 3))
      (observed (geometric-parallel-observation '(1 2 3) 2 '(complex quaternionic))))
  ;; Process results...
  )

;; Hopf-based consciousness
(let ((conscious (consciousness-hopf-project unconscious-state 'complex))
      (parallel (parallel-observation state '(complex quaternionic octonionic)))
      (bound (fiber-binding parallel)))
  ;; Unified conscious experience
  )

;; Formal dynamics
(let ((evolved (evolve-consciousness-state '(5.0 0.7 0.8) 0.01 '())))
  ;; Evolved state
  )
```

**Research**: See `dev-docs/research/25-Hopf-Fibrations/` for detailed theory.

## Use Cases

For detailed real-world use cases and applications, see [MLSS Use Cases](MLSS_USE_CASES).

Common use cases include:
- **Data Processing**: Universal format conversion, content-addressed storage
- **Signal Processing**: Waveform synthesis, audio feature extraction
- **Computer Vision**: Image search, multi-view geometry
- **Optimal Decision Making**: Pathfinding, resource allocation
- **Consciousness Research**: Conscious state simulation, learning models, geometric consciousness theory
- **Computational Physics**: Quantum simulation, spacetime computation
- **Knowledge Representation**: Symbolic fact extraction, cross-domain reasoning
- **Distributed Systems**: Content-addressed federation, provenance tracking

## Next Steps

1. Review [MLSS Use Cases](MLSS_USE_CASES) for real-world applications
2. Review the [Integration Specification](../dev-docs/INTEGRATION-MLSS.md)
3. Run the [quick demo](../tests/demo-mlss-quick.sh)
4. Explore the [verification log](../tests/VERIFICATION-LOG.md)
5. Check [benchmark results](../tests/BENCHMARK-RESULTS.md)

---

For detailed API reference, see the Scheme module files in `scheme/` directory.

