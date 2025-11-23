#!/bin/bash
# Run All MLSS Tests and Benchmarks
set -e

echo "=========================================="
echo "MLSS Complete Test Suite"
echo "=========================================="
echo ""

echo "1. Running unit tests..."
echo "   - Substrate tests"
guile -c "(load \"scheme/r5rs-canvas-engine.scm\") (load \"scheme/substrate/runtime.test.scm\")" 2>&1 | grep -E "(Test|✓|✗|All)" | head -5

echo ""
echo "2. Running integration tests..."
./tests/test-integration-workflow.sh 2>&1 | grep -E "(Test|✓|✗|PASSED|FAILED)" | head -10

echo ""
echo "3. Running Q* tests..."
./tests/test-qstar-workflow.sh 2>&1 | grep -E "(Test|✓|✗|PASSED|FAILED)" | head -5

echo ""
echo "4. Running consciousness tests..."
./tests/test-consciousness-workflow.sh 2>&1 | grep -E "(Test|✓|✗|PASSED|FAILED)" | head -5

echo ""
echo "5. Running physics tests..."
./tests/test-physics-workflow.sh 2>&1 | grep -E "(Test|✓|✗|PASSED|FAILED)" | head -5

echo ""
echo "=========================================="
echo "All tests complete!"
echo "=========================================="
echo ""
echo "To run benchmarks: ./tests/benchmark-mlss.sh"
echo "To run demo: ./tests/demo-mlss.sh"
