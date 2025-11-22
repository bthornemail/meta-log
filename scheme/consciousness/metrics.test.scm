;;; consciousness/metrics.test.scm --- Consciousness Metrics Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for consciousness quality metrics

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../consciousness/state.scm")
(load "../consciousness/qualia.scm")
(load "../consciousness/metrics.scm")

;; Test 1: Metrics Creation
(display "Test 1: Metrics Creation\n")
(let* ((creativity 1.5)
       (focus 0.8)
       (coherence 0.7)
       (richness 0.6)
       (learning 0.3)
       (metrics (make-consciousness-metrics creativity focus coherence richness learning)))
  (if (and (list? metrics)
           (= (length metrics) 8)
           (eq? (list-ref metrics 0) 'consciousness-metrics))
      (display "  ✓ Metrics creation works\n")
      (begin (display "  ✗ Metrics creation failed\n") (exit 1))))

;; Test 2: Creativity Index
(display "Test 2: Creativity Index\n")
(let* ((current 10.0)
       (previous 5.0)
       (creativity (compute-creativity-index current previous)))
  (if (and (number? creativity)
           (> creativity 1.0))
      (display "  ✓ Creativity index computation works\n")
      (begin (display "  ✗ Creativity index computation failed\n") (exit 1))))

;; Test 3: Focus Efficiency
(display "Test 3: Focus Efficiency\n")
(let* ((selected 4)
       (total 10)
       (efficiency (compute-focus-efficiency selected total)))
  (if (and (number? efficiency)
           (>= efficiency 0.0)
           (<= efficiency 1.0))
      (display "  ✓ Focus efficiency computation works\n")
      (begin (display "  ✗ Focus efficiency computation failed\n") (exit 1))))

;; Test 4: Coherence Stability
(display "Test 4: Coherence Stability\n")
(let* ((phase-history '(0.7 0.8 0.75 0.79 0.77))
       (stability (compute-coherence-stability phase-history)))
  (if (and (number? stability)
           (>= stability 0.0)
           (<= stability 1.0))
      (display "  ✓ Coherence stability computation works\n")
      (begin (display "  ✗ Coherence stability computation failed\n") (exit 1))))

;; Test 5: Consciousness Quality Metric (CQM)
(display "Test 5: Consciousness Quality Metric\n")
(let* ((metrics (make-consciousness-metrics 1.5 0.8 0.7 0.6 0.3))
       (weights '((action . 1.0) (observation . 1.0) (coherence . 1.0) (qualia . 1.0) (learning . 1.0)))
       (cqm (compute-cqm metrics weights)))
  (if (and (number? cqm)
           (> cqm 0.0))
      (display "  ✓ CQM computation works\n")
      (begin (display "  ✗ CQM computation failed\n") (exit 1))))

(display "\nAll consciousness metrics tests passed!\n")

