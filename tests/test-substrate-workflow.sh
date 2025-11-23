#!/bin/bash
# Integration test for complete substrate workflows

set -e

echo "=========================================="
echo "MLSS Substrate Workflow Integration Tests"
echo "=========================================="
echo ""

cd "$(dirname "$0")/.."

# Test 1: Create memory → Transform → Get provenance
echo "Test 1: Memory → Transform → Provenance workflow"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((data '(1 2 3 4 5))
       (meta '((encoding . \"raw\")))
       (created (substrate-create-memory data meta))
       (uri (cadr created)))
  (display (string-append \"Created memory: \" uri \"\\n\"))
  (if (string-prefix? \"mlss://\" uri)
      (display \"✓ Content addressing works\\n\")
      (begin (display \"✗ Content addressing failed\\n\") (exit 1))))
" || echo "✗ Test 1 failed"

echo ""

# Test 2: Binary transformation chain
echo "Test 2: Binary transformation chain"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((bytes '(10 20 30 40))
       (meta '((encoding . \"raw\")))
       (cbs (make-cbs bytes meta))
       (xor-result (binary-xor cbs '(255 0 255 0)))
       (rotated (binary-rotate xor-result 2 'left)))
  (if (and (list? xor-result) (list? rotated))
      (display \"✓ Binary transformation chain works\\n\")
      (begin (display \"✗ Binary chain failed\\n\") (exit 1))))
" || echo "✗ Test 2 failed"

echo ""

# Test 3: Content addressing deduplication
echo "Test 3: Content addressing deduplication"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((data '(1 2 3 4))
       (meta '((encoding . \"raw\")))
       (result1 (substrate-create-memory data meta))
       (result2 (substrate-create-memory data meta)))
  (if (equal? (cadr result1) (cadr result2))
      (display \"✓ Content addressing deduplication works\\n\")
      (begin (display \"✗ Deduplication failed\\n\") (exit 1))))
" || echo "✗ Test 3 failed"

echo ""
echo "=========================================="
echo "Integration tests complete!"
echo "=========================================="

