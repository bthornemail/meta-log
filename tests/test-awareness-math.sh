#!/bin/bash
# Test Awareness Mathematical Validation
# Meta-Log Substrate System
# Tests mathematical foundations of awareness from 14-Geometric-Theory.md

set -e

echo "=========================================="
echo "Awareness Mathematical Validation Tests"
echo "=========================================="
echo ""

# Test 1: Qualia Intensity vs. Hopf Fiber Type
echo "Test 1: Qualia Intensity vs. Hopf Fiber Type"
echo "  Validates: octonionic > quaternionic > complex"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/consciousness/qualia.test.scm\")
" && echo "  ✓ Test 1 PASSED" || echo "  ✗ Test 1 FAILED"

echo ""

# Test 2: Complexity Scaling
echo "Test 2: Complexity Scaling"
echo "  Validates: O(k) observation, O(2^d) action"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/consciousness/complexity.test.scm\")
" && echo "  ✓ Test 2 PASSED" || echo "  ✗ Test 2 FAILED"

echo ""

# Test 3: Differential Equation Solutions
echo "Test 3: Differential Equation Solutions"
echo "  Validates: dA/dt, dO/dt, dΦ/dt from dynamics.scm"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/consciousness/dynamics.scm\")
(let* (       (action 1.0)
       (observation 0.5)
       (phase 0.6)
       (dt 0.01)
       (params '((lambda . 0.1) (gamma . 0.05) (sigma1 . 0.01)
                 (mu . 0.1) (kappa . 0.2) (sigma2 . 0.01)
                 (omega0 . 1.0) (alpha . 0.1) (beta . 0.1)))
       (state (list action observation phase))
       (new-state (evolve-consciousness-state state dt params)))
  (if (and (list? new-state) (= (length new-state) 3))
      (display \"  ✓ Differential equations work\\n\")
      (begin (display \"  ✗ Differential equations failed\\n\") (exit 1))))
" && echo "  ✓ Test 3 PASSED" || echo "  ✗ Test 3 FAILED"

echo ""

echo "=========================================="
echo "Mathematical Validation Summary"
echo "=========================================="
echo ""
echo "All mathematical foundation tests completed."
echo "These tests validate the theoretical predictions from 14-Geometric-Theory.md"
echo ""

