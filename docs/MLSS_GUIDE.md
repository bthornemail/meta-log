---
layout: default
title: MLSS Guide
nav_order: 5
description: "Meta-Log Substrate System (MLSS) - Complete guide"
permalink: /MLSS_GUIDE
---

# Meta-Log Substrate System (MLSS) Guide

Complete guide to the Meta-Log Substrate System - a unified computational architecture that bridges binary, waveform, geometric, and symbolic cognition.

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

**Features**:
- Exponential action / Linear observation dynamics
- Qualia emergence from state tension
- Consciousness Quality Metric (CQM)
- Metrics collection and history

### Phase 6: Computational Physics

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

## Use Cases

For detailed real-world use cases and applications, see [MLSS Use Cases](MLSS_USE_CASES).

Common use cases include:
- **Data Processing**: Universal format conversion, content-addressed storage
- **Signal Processing**: Waveform synthesis, audio feature extraction
- **Computer Vision**: Image search, multi-view geometry
- **Optimal Decision Making**: Pathfinding, resource allocation
- **Consciousness Research**: Conscious state simulation, learning models
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

