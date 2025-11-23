# MLSS Quick Start Guide

## Running Tests

### All Tests
```bash
./tests/run-all-tests.sh
```

### Individual Test Suites
```bash
# Foundation tests
./tests/test-substrate-workflow.sh

# Integration tests
./tests/test-integration-workflow.sh

# Q* tests
./tests/test-qstar-workflow.sh

# Consciousness tests
./tests/test-consciousness-workflow.sh

# Physics tests
./tests/test-physics-workflow.sh
```

## Running Demos

### Quick Demo (Non-Interactive)
```bash
./tests/demo-mlss-quick.sh
```

### Full Interactive Demo
```bash
./tests/demo-mlss.sh
```

## Running Benchmarks

### Full Benchmark Suite
```bash
./tests/benchmark-mlss.sh
```

Results saved to: `tests/BENCHMARK-RESULTS.md`

## Documentation

- **Integration Spec**: `dev-docs/INTEGRATION-MLSS.md`
- **Benchmark Guide**: `tests/README-BENCHMARKS.md`
- **Test Reference**: `tests/TESTING-STATUS.md`

## System Status

All 6 phases implemented and tested:
- ✓ Phase 1: Foundation
- ✓ Phase 2: Waveform & Geometric
- ✓ Phase 3: Q* Optimality Engine
- ✓ Phase 4: Computer Vision
- ✓ Phase 5: Consciousness Framework
- ✓ Phase 6: Computational Physics
