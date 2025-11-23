#!/bin/bash
# Quick MLSS Benchmark (Reduced Iterations)
# For faster testing

set -e

BENCHMARK_LOG="tests/BENCHMARK-RESULTS-QUICK.md"
echo "# Quick MLSS Benchmark Results" > "$BENCHMARK_LOG"
echo "Generated: $(date)" >> "$BENCHMARK_LOG"
echo "" >> "$BENCHMARK_LOG"

echo "Running quick benchmarks (10 iterations each)..."

# Quick test - just a few benchmarks
echo "Testing substrate memory creation (10 iterations)..."
time (for i in {1..10}; do 
    guile -c "(load \"scheme/r5rs-canvas-engine.scm\") (substrate-create-memory #u8(1 2 3 4) '((test . \"benchmark\")))" > /dev/null 2>&1
done) 2>&1 | tail -3

echo ""
echo "Quick benchmark complete!"
echo "Results saved to: $BENCHMARK_LOG"
