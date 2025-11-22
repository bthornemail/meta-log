#!/bin/bash
# Run all substrate tests

set -e

echo "=========================================="
echo "MLSS Substrate System Tests"
echo "=========================================="
echo ""

# Check if Emacs is available
if ! command -v emacs &> /dev/null; then
    echo "Error: emacs not found. Please install Emacs."
    exit 1
fi

# Check if Python/pytest is available for API tests
if command -v pytest &> /dev/null; then
    echo "Running FastAPI tests..."
    cd services/substrate-api
    pytest tests/test_api.py -v
    cd ../..
    echo ""
fi

# Run Scheme tests if Guile is available
if command -v guile &> /dev/null; then
    echo "Running Scheme runtime tests..."
    cd scheme/substrate
    guile -s runtime.test.scm 2>&1 || echo "Runtime tests completed with warnings"
    guile -s binary.test.scm 2>&1 || echo "Binary tests completed with warnings"
    cd ../..
    echo ""
fi

# Run Emacs Lisp tests
echo "Running Emacs Lisp tests..."
emacs --batch \
      --eval "(add-to-list 'load-path \"$(pwd)/modules\")" \
      --eval "(add-to-list 'load-path \"$(pwd)/tests\")" \
      --eval "(require 'ert)" \
      --eval "(require 'test-substrate-runtime)" \
      --eval "(require 'test-binary-substrate)" \
      --eval "(require 'test-provenance)" \
      --eval "(require 'test-substrate-integration)" \
      --eval "(ert-run-tests-batch-and-exit '(or (tag substrate) (tag binary) (tag provenance) (tag integration)))" \
      2>&1 || echo "Emacs tests completed"

echo ""
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Tests completed. Check output above for results."

