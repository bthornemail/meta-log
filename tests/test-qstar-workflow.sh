#!/bin/bash
# test-qstar-workflow.sh --- Q* Integration Workflow Tests
# Meta-Log Substrate System
# Copyright (C) 2025 Meta-Log Research Group

set -e

echo "=========================================="
echo "Q* Optimality Engine Integration Tests"
echo "=========================================="
echo ""

# Test 1: Q* Core Functionality
echo "Test 1: Q* Core Evaluation"
echo "  State creation → Action evaluation → Policy selection"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((state (make-qstar-state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ()))
                                '((total-memory-bytes . 1000) (total-entropy . 0.7) (consistency-score . 0.9))))
       (action1 (make-qstar-action 'transform 'binary-xor '((mask . #u8(255)))))
       (action2 (make-qstar-action 'transform 'binary-rotate '((n-bits . 2) (direction . left))))
       (eval1 (qstar-evaluate state action1))
       (eval2 (qstar-evaluate state action2))
       (policy (qstar-policy state (list action1 action2))))
  (if (and (list? policy) (= (length policy) 3))
      (display \"  ✓ Q* core workflow works\\n\")
      (begin (display \"  ✗ Q* core workflow failed\\n\") (exit 1))))
" && echo "  ✓ Test 1 PASSED" || echo "  ✗ Test 1 FAILED"

echo ""

# Test 2: Q* with Substrate Integration
echo "Test 2: Q* Substrate Integration"
echo "  Create substrate memory → Evaluate with Q*"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((bytes '(10 20 30 40 50))
       (meta '((encoding . \"raw\")))
       (memory-result (substrate-create-memory bytes meta))
       (memory-obj (list-ref memory-result 0))
       (memory-uri (list-ref memory-result 1))
       (state (make-qstar-state (list (cons 'binary (list memory-uri)) (cons 'waveform '()) (cons 'geometric '()) (cons 'symbolic '()))
                                '((total-memory-bytes . 5) (total-entropy . 0.5) (consistency-score . 1.0))))
       (action (make-qstar-action 'transform 'binary-xor '((mask . #u8(255)))))
       (result (qstar-evaluate state action)))
  (if (and (list? result) (= (length result) 3))
      (display \"  ✓ Q* substrate integration works\\n\")
      (begin (display \"  ✗ Q* substrate integration failed\\n\") (exit 1))))
" && echo "  ✓ Test 2 PASSED" || echo "  ✗ Test 2 FAILED"

echo ""

# Test 3: Scoring Function Registry
echo "Test 3: Scoring Function Registry"
echo "  Register custom function → Compute composite score"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((custom-fn (lambda (s a s2) 2.5))
       (before-count (length *scoring-registry*))
       (registered (qstar-register-scoring-fn 'custom-test custom-fn 25))
       (after-count (length *scoring-registry*))
       (state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ())))
       (action '(transform binary-xor))
       (next-state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ())))
       (scores (qstar-compute-composite-score state action next-state)))
  (if (and (> after-count before-count)
           (assoc-ref scores 'custom-test)
           (= (assoc-ref scores 'custom-test) 2.5))
      (display \"  ✓ Scoring registry works\\n\")
      (begin (display \"  ✗ Scoring registry failed\\n\") (exit 1))))
" && echo "  ✓ Test 3 PASSED" || echo "  ✗ Test 3 FAILED"

echo ""
echo "=========================================="
echo "Q* integration workflow tests complete!"
echo "=========================================="

