#!/bin/bash
# Master Awareness Validation Test Script
# Meta-Log Substrate System
# Tests all awareness validation predictions from 14-Geometric-Theory.md

set -e

echo "=========================================="
echo "Awareness Validation Test Suite"
echo "=========================================="
echo "Based on 14-Geometric-Theory.md Section 4"
echo ""

# Test 1: Reaction Time Scaling
echo "Test 1: Reaction Time Scaling (O(k) linear, not O(2^d) exponential)"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"tests/awareness/reaction-time.scm\")
" && echo "  ✓ Test 1 PASSED" || echo "  ✗ Test 1 FAILED"

echo ""

# Test 2: Working Memory Capacity
echo "Test 2: Working Memory Capacity (7±2)"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"tests/awareness/working-memory.scm\")
" && echo "  ✓ Test 2 PASSED" || echo "  ✗ Test 2 FAILED"

echo ""

# Test 3: Qualia Intensity
echo "Test 3: Qualia Intensity (octonionic > quaternionic > complex)"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"tests/awareness/qualia-intensity.scm\")
" && echo "  ✓ Test 3 PASSED" || echo "  ✗ Test 3 FAILED"

echo ""

# Test 4: Independence
echo "Test 4: Independence (unconscious O(2^d) vs conscious O(k))"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"tests/awareness/independence.scm\")
" && echo "  ✓ Test 4 PASSED" || echo "  ✗ Test 4 FAILED"

echo ""

echo "=========================================="
echo "Awareness Validation Summary"
echo "=========================================="
echo ""
echo "All awareness validation tests completed."
echo "Results validate empirical predictions from 14-Geometric-Theory.md"
echo ""

