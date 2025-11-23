# MLSS Testing Guide

## Overview

This directory contains comprehensive tests for the Meta-Log Substrate System (MLSS) implementation.

## Test Structure

### Emacs Lisp Tests

- `test-substrate-runtime.el` - Substrate Runtime Protocol tests
- `test-binary-substrate.el` - Binary Substrate operations tests
- `test-provenance.el` - Provenance Chain Protocol tests
- `test-substrate-integration.el` - End-to-end integration tests

### Scheme Tests

- `scheme/substrate/runtime.test.scm` - Runtime unit tests
- `scheme/substrate/binary.test.scm` - Binary operations unit tests

### FastAPI Tests

- `services/substrate-api/tests/test_api.py` - API endpoint tests

### Test Scripts

- `run-substrate-tests.sh` - Run all tests
- `test-scheme-substrate.sh` - Quick Scheme module test
- `test-fastapi-substrate.sh` - Quick API service test

## Running Tests

### All Tests

```bash
./tests/run-substrate-tests.sh
```

### Emacs Lisp Tests Only

```elisp
M-x ert RET substrate-runtime-tests RET
M-x ert RET binary-substrate-tests RET
M-x ert RET provenance-tests RET
M-x ert RET substrate-integration-tests RET
```

Or in batch mode:

```bash
emacs --batch \
      --eval "(add-to-list 'load-path \"$(pwd)/modules\")" \
      --eval "(add-to-list 'load-path \"$(pwd)/tests\")" \
      --eval "(require 'ert)" \
      --eval "(require 'test-substrate-runtime)" \
      --eval "(ert-run-tests-batch-and-exit '(tag substrate))"
```

### Scheme Tests Only

```bash
./tests/test-scheme-substrate.sh
```

Or manually:

```bash
cd scheme/substrate
guile -s runtime.test.scm
guile -s binary.test.scm
```

### FastAPI Tests Only

First, start the service:

```bash
cd services/substrate-api
uvicorn app:app --port 8001
```

Then in another terminal:

```bash
./tests/test-fastapi-substrate.sh
```

Or with pytest:

```bash
cd services/substrate-api
pytest tests/test_api.py -v
```

## Test Coverage

### Phase 1: Foundation ✓

- [x] Substrate Runtime Protocol (SRP)
  - [x] Memory object creation
  - [x] Content addressing (mlss:// URIs)
  - [x] URI resolution
  - [x] Content deduplication

- [x] Binary Layer Protocol (BLP)
  - [x] CBS creation
  - [x] XOR transformation
  - [x] Bit rotation
  - [x] Slice extraction
  - [x] Concatenation

- [x] Provenance Chain Protocol (PCP)
  - [x] Provenance retrieval
  - [x] Chain verification
  - [x] Transformation tracking

### Phase 2: Waveform (Basic) ✓

- [x] Waveform structure
- [x] WDL parsing (basic)
- [x] Cross-domain mappings (structure)

### Phase 3: Q* (Basic) ✓

- [x] Q* evaluation structure
- [x] Cost function registry
- [x] A* pathfinding structure

## Expected Results

### Successful Test Run

```
==========================================
MLSS Substrate System Tests
==========================================

Running FastAPI tests...
test_api.py::test_health_check PASSED
test_api.py::test_hash_endpoint PASSED
test_api.py::test_compress_endpoint PASSED

Running Scheme runtime tests...
Testing UUID generation... PASS
Testing content hash... PASS
Testing memory object creation... PASS
Testing content addressing... PASS

Running Emacs Lisp tests...
Running 12 tests
PASSED:  12
FAILED:  0

==========================================
Test Summary
==========================================
All tests passed!
```

## Troubleshooting

### Emacs Tests Fail

- Ensure `meta-log-substrate-runtime.el` is in load-path
- Check that `meta-log-r5rs.el` can load Scheme files
- Verify Geiser is installed and configured

### Scheme Tests Fail

- Install Guile 3.0+: `apt-get install guile-3.0` or `brew install guile`
- Check that Scheme files are in correct paths
- Some functions require Guile extensions (bytevector, hash-table)

### FastAPI Tests Fail

- Start the service: `cd services/substrate-api && uvicorn app:app --port 8001`
- Check port 8001 is not in use
- Verify Python dependencies: `pip install -r requirements.txt`

## Continuous Integration

For CI/CD, run:

```bash
# Install dependencies
pip install -r services/substrate-api/requirements.txt
pip install pytest

# Run all tests
./tests/run-substrate-tests.sh
```

## Next Steps

After basic tests pass:

1. Add property-based tests (determinism, reversibility)
2. Add performance benchmarks
3. Add integration tests for cross-domain mappings
4. Add Q* evaluation tests
5. Add consciousness metrics tests

---

**See also:**
- `dev-docs/INTEGRATION-MLSS.md` - Integration specification
- `dev-docs/MLSS-IMPLEMENTATION-SUMMARY.md` - Implementation details

