#!/bin/bash
# Complete integration workflow tests

set -e

echo "=========================================="
echo "MLSS Complete Integration Workflow Tests"
echo "=========================================="
echo ""

cd "$(dirname "$0")/.."

# Test 1: Full substrate workflow
echo "Test 1: Complete Substrate Workflow"
echo "  Creating memory → Transforming → Getting provenance"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((data '(1 2 3 4 5))
       (meta '((encoding . \"raw\")))
       (created (substrate-create-memory data meta))
       (uri (cadr created))
       (obj (car created)))
  (display (string-append \"  ✓ Created: \" uri \"\\n\"))
  (if (string-prefix? \"mlss://\" uri)
      (display \"  ✓ Content addressing works\\n\")
      (begin (display \"  ✗ Content addressing failed\\n\") (exit 1))))
" && echo "  ✓ Test 1 PASSED" || echo "  ✗ Test 1 FAILED"

echo ""

# Test 2: Binary transformation pipeline
echo "Test 2: Binary Transformation Pipeline"
echo "  CBS → XOR → Rotate → Slice"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((bytes '(10 20 30 40 50))
       (meta '((encoding . \"raw\")))
       (cbs (make-cbs bytes meta))
       (xor-result (binary-xor cbs '(255 0 255 0)))
       (rotated (binary-rotate xor-result 2 'left))
       (sliced (binary-slice rotated 1 3)))
  (if (and (list? xor-result) (list? rotated) (list? sliced))
      (display \"  ✓ Binary pipeline works\\n\")
      (begin (display \"  ✗ Binary pipeline failed\\n\") (exit 1))))
" && echo "  ✓ Test 2 PASSED" || echo "  ✗ Test 2 FAILED"

echo ""

# Test 3: Content addressing deduplication
echo "Test 3: Content Addressing Deduplication"
echo "  Same data → Same URI"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((data '(1 2 3 4))
       (meta '((encoding . \"raw\")))
       (result1 (substrate-create-memory data meta))
       (result2 (substrate-create-memory data meta)))
  (if (equal? (cadr result1) (cadr result2))
      (display \"  ✓ Deduplication works\\n\")
      (begin (display \"  ✗ Deduplication failed\\n\") (exit 1))))
" && echo "  ✓ Test 3 PASSED" || echo "  ✗ Test 3 FAILED"

echo ""

# Test 4: Waveform creation (basic)
echo "Test 4: Waveform Creation"
echo "  Creating waveform from samples"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((samples '(0.1 0.2 0.3 0.2 0.1))
       (sample-rate 48000)
       (duration 0.1)
       (channels 1)
       (waveform (waveform-create samples sample-rate duration channels)))
  (if (and (list? waveform) (eq? (car waveform) 'waveform-substrate))
      (display \"  ✓ Waveform creation works\\n\")
      (begin (display \"  ✗ Waveform creation failed\\n\") (exit 1))))
" && echo "  ✓ Test 4 PASSED" || echo "  ✗ Test 4 FAILED"

echo ""

# Test 5: Cross-domain mapping (Binary → Waveform)
echo "Test 5: Cross-Domain Mapping"
echo "  Binary → Waveform transformation"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((bytes '(128 150 170 150 128))
       (meta '((encoding . \"raw\")))
       (cbs (make-cbs bytes meta))
       (hash (list-ref cbs 5))
       (uri (content-address hash))
       (stored (store-memory-object cbs))
       (params '((sample-rate . 48000) (bit-depth . 8) (channels . 1)))
       (waveform (binary-to-waveform uri 'direct params)))
  (if (and (list? waveform) (>= (length waveform) 2))
      (display \"  ✓ Cross-domain mapping works\\n\")
      (begin (display \"  ✗ Cross-domain mapping failed\\n\") (exit 1))))
" && echo "  ✓ Test 5 PASSED" || echo "  ✗ Test 5 FAILED"

echo ""
echo "=========================================="
echo "Integration workflow tests complete!"
echo "=========================================="

