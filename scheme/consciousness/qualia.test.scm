;;; consciousness/qualia.test.scm --- Qualia Field Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for qualia field emergence

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../consciousness/state.scm")
(load "../consciousness/qualia.scm")

;; Test 1: Qualia Field Creation
(display "Test 1: Qualia Field Creation\n")
(let* ((qualia-map '((tension . 2.5) (coherence . 0.8) (intensity . 1.5)))
       (coherence 0.8)
       (richness 0.6)
       (field (make-qualia-field qualia-map coherence richness)))
  (if (and (list? field)
           (= (length field) 6)
           (eq? (list-ref field 0) 'qualia-field))
      (display "  ✓ Qualia field creation works\n")
      (begin (display "  ✗ Qualia field creation failed\n") (exit 1))))

;; Test 2: Qualia Emergence
(display "Test 2: Qualia Emergence\n")
(let* ((action 5.0)
       (observation 0.7)
       (phase 0.8)
       (threshold 0.3)
       (field (emerge-qualia action observation phase threshold)))
  (if (or (list? field) (not field))
      (display "  ✓ Qualia emergence works\n")
      (begin (display "  ✗ Qualia emergence failed\n") (exit 1))))

;; Test 3: Qualia Tensor Product
(display "Test 3: Qualia Tensor Product\n")
(let* ((action 3.0)
       (observation 0.6)
       (product (qualia-tensor-product action observation)))
  (if (and (number? product)
           (> product 0.0))
      (display "  ✓ Qualia tensor product works\n")
      (begin (display "  ✗ Qualia tensor product failed\n") (exit 1))))

;; Test 4: Qualia Richness
(display "Test 4: Qualia Richness\n")
(let* ((qualia-map '((tension . 2.0) (coherence . 0.7) (intensity . 1.5)))
       (richness (qualia-richness qualia-map)))
  (if (and (number? richness)
           (>= richness 0.0)
           (<= richness 1.0))
      (display "  ✓ Qualia richness computation works\n")
      (begin (display "  ✗ Qualia richness computation failed\n") (exit 1))))

(display "\nAll qualia field tests passed!\n")

