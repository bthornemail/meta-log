#!/bin/bash
# Test Computational Physics Integration Workflow
# Meta-Log Substrate System

set -e

echo "=========================================="
echo "Computational Physics Integration Tests"
echo "=========================================="
echo ""

# Test 1: Quantum State Creation
echo "Test 1: Quantum State Creation"
echo "  Create quantum state → Encode as CBS → Compute probabilities"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((qubits 2)
       (wavefunction '(0.707 0.707 0.0 0.0))
       (result (physics-quantum-create qubits wavefunction))
       (quantum (list-ref result 0))
       (uri (list-ref result 1))
       (probabilities (quantum-compute-probabilities (list-ref quantum 2))))
  (if (and (list? quantum) (string? uri) (list? probabilities))
      (display \"  ✓ Quantum state workflow works\\n\")
      (begin (display \"  ✗ Quantum state workflow failed\\n\") (exit 1))))
" && echo "  ✓ Test 1 PASSED" || echo "  ✗ Test 1 FAILED"

echo ""

# Test 2: E8 to GR Mapping
echo "Test 2: E8 to General Relativity"
echo "  E8 energy-momentum → Einstein equations → Metric tensor"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((energy-momentum '((1.0 0.0 0.0 0.0) (0.0 1.0 0.0 0.0) (0.0 0.0 1.0 0.0) (0.0 0.0 0.0 1.0)))
       (equations (einstein-equations energy-momentum))
       (metric (e8-to-metric energy-momentum)))
  (if (and (list? equations) (list? metric))
      (display \"  ✓ E8 to GR mapping works\\n\")
      (begin (display \"  ✗ E8 to GR mapping failed\\n\") (exit 1))))
" && echo "  ✓ Test 2 PASSED" || echo "  ✗ Test 2 FAILED"

echo ""

# Test 3: p-adic to QFT
echo "Test 3: p-adic to Quantum Field Theory"
echo "  p-adic valuation → Field configuration → CBS encoding"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((padic-valuation '(1 2 3 4))
       (prime 3)
       (depth 4)
       (field (padic-to-field-theory padic-valuation prime depth))
       (cbs-result (field-to-cbs field))
       (uri (list-ref cbs-result 1)))
  (if (and (list? field) (string? uri))
      (display \"  ✓ p-adic to QFT mapping works\\n\")
      (begin (display \"  ✗ p-adic to QFT mapping failed\\n\") (exit 1))))
" && echo "  ✓ Test 3 PASSED" || echo "  ✗ Test 3 FAILED"

echo ""
echo "=========================================="
echo "Computational physics workflow tests complete!"
echo "=========================================="

