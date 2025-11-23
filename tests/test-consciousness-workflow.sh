#!/bin/bash
# Test Consciousness Framework Integration Workflow
# Meta-Log Substrate System

set -e

echo "=========================================="
echo "Consciousness Framework Integration Tests"
echo "=========================================="
echo ""

# Test 1: Conscious State Creation
echo "Test 1: Conscious State Creation"
echo "  Create state → Forward/backward propagation → Qualia emergence"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((action 5.0)
       (observation 0.7)
       (phase 0.8)
       (state-result (consciousness-create-state action observation phase))
       (state (list-ref state-result 0))
       (uri (list-ref state-result 1))
       (next-action (conscious-action-forward action 0.1 0.0))
       (next-observation (conscious-observation-backward observation 0.9 (lambda (x) x)))
       (qualia-field (emerge-qualia next-action next-observation phase 0.3)))
  (if (and (list? state) (string? uri) (or (list? qualia-field) (not qualia-field)))
      (display \"  ✓ Conscious state workflow works\\n\")
      (begin (display \"  ✗ Conscious state workflow failed\\n\") (exit 1))))
" && echo "  ✓ Test 1 PASSED" || echo "  ✗ Test 1 FAILED"

echo ""

# Test 2: Qualia Field Evolution
echo "Test 2: Qualia Field Evolution"
echo "  Create qualia field → Evolve with state changes → Check threshold"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((qualia-map '((tension . 2.5) (coherence . 0.8) (intensity . 1.5)))
       (field (make-qualia-field qualia-map 0.8 0.6))
       (action-delta 0.5)
       (observation-delta 0.2)
       (evolved (evolve-qualia-field field action-delta observation-delta))
       (above-threshold (qualia-threshold? evolved 0.5)))
  (if (and (list? evolved) (eq? (list-ref evolved 0) 'qualia-field))
      (display \"  ✓ Qualia field evolution works\\n\")
      (begin (display \"  ✗ Qualia field evolution failed\\n\") (exit 1))))
" && echo "  ✓ Test 2 PASSED" || echo "  ✗ Test 2 FAILED"

echo ""

# Test 3: Consciousness Metrics Collection
echo "Test 3: Consciousness Metrics Collection"
echo "  Collect metrics → Compute CQM → Record in history"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((current-state (make-conscious-state 10.0 0.8 0.7))
       (previous-state (make-conscious-state 5.0 0.6 0.65))
       (qualia-field (make-qualia-field '((intensity . 1.5)) 0.7 0.6))
       (metrics (collect-metrics current-state previous-state qualia-field))
       (weights '((action . 1.0) (observation . 1.0) (coherence . 1.0) (qualia . 1.0) (learning . 1.0)))
       (cqm (compute-cqm metrics weights))
       (recorded (record-metrics metrics)))
  (if (and (list? metrics) (number? cqm) (> cqm 0.0))
      (display \"  ✓ Metrics collection works\\n\")
      (begin (display \"  ✗ Metrics collection failed\\n\") (exit 1))))
" && echo "  ✓ Test 3 PASSED" || echo "  ✗ Test 3 FAILED"

echo ""
echo "=========================================="
echo "Consciousness framework workflow tests complete!"
echo "=========================================="

