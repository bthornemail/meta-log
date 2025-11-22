;;; qstar/core.test.scm --- Q* Core Engine Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for Q* core optimality engine

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../qstar/core.scm")

;; Test 1: State Creation
(display "Test 1: Q* State Creation\n")
(let ((state (make-qstar-state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ()))
                               '((total-memory-bytes . 0) (total-entropy . 0.0) (consistency-score . 1.0)))))
  (if (and (list? state)
           (= (length state) 4)
           (eq? (list-ref state 0) 'qstar-state))
      (display "  ✓ State creation works\n")
      (begin (display "  ✗ State creation failed\n") (exit 1))))

;; Test 2: Action Creation
(display "Test 2: Q* Action Creation\n")
(let ((action (make-qstar-action 'transform 'binary-xor '((mask . #u8(255))))))
  (if (and (list? action)
           (= (length action) 4)
           (eq? (list-ref action 0) 'qstar-action)
           (eq? (list-ref action 1) 'transform))
      (display "  ✓ Action creation works\n")
      (begin (display "  ✗ Action creation failed\n") (exit 1))))

;; Test 3: Q-Value Evaluation
(display "Test 3: Q-Value Evaluation\n")
(let* ((test-state (make-qstar-state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ()))
                                     '((total-memory-bytes . 100) (total-entropy . 0.5) (consistency-score . 0.8))))
       (test-action (make-qstar-action 'transform 'binary-xor '((mask . #u8(255)))))
       (result (qstar-evaluate test-state test-action)))
  (if (and (list? result)
           (= (length result) 3)
           (number? (list-ref result 0))  ; q-value
           (list? (list-ref result 1))    ; plan
           (list? (list-ref result 2)))   ; provenance
      (display "  ✓ Q-value evaluation works\n")
      (begin (display "  ✗ Q-value evaluation failed\n") (exit 1))))

;; Test 4: Policy Selection
(display "Test 4: Policy Selection\n")
(let* ((test-state (make-qstar-state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ()))
                                     '((total-memory-bytes . 100) (total-entropy . 0.5) (consistency-score . 0.8))))
       (test-actions (list (make-qstar-action 'transform 'binary-xor '((mask . #u8(255))))
                           (make-qstar-action 'transform 'binary-rotate '((n-bits . 2) (direction . left)))
                           (make-qstar-action 'synthesize 'waveform-create '((samples . (128 150 170))))))
       (policy (qstar-policy test-state test-actions)))
  (if (and (list? policy)
           (= (length policy) 3)
           (list? (list-ref policy 0))  ; best action
           (number? (list-ref policy 1))  ; value
           (list? (list-ref policy 2)))   ; candidates
      (display "  ✓ Policy selection works\n")
      (begin (display "  ✗ Policy selection failed\n") (exit 1))))

;; Test 5: Cost Computation
(display "Test 5: Cost Computation\n")
(let* ((test-state (make-qstar-state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ()))
                                     '((total-memory-bytes . 1000) (total-entropy . 0.7) (consistency-score . 0.9))))
       (test-action (make-qstar-action 'transform 'binary-xor '((mask . #u8(255)))))
       (costs (qstar-compute-costs test-state test-action)))
  (if (and (list? costs)
           (assoc-ref costs 'computational)
           (assoc-ref costs 'memory)
           (assoc-ref costs 'entropy)
           (assoc-ref costs 'complexity)
           (assoc-ref costs 'safety))
      (display "  ✓ Cost computation works\n")
      (begin (display "  ✗ Cost computation failed\n") (exit 1))))

(display "\nAll Q* core tests passed!\n")

