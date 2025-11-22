;;; physics/qft.test.scm --- Quantum Field Theory Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for Quantum Field Theory

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../substrate/binary.scm")
(load "../physics/qft.scm")

;; Helper: make-list
(define (make-list n value)
  "Create list of n elements with value."
  (let loop ((i 0) (result '()))
    (if (>= i n)
        (reverse result)
        (loop (+ i 1) (cons value result)))))

;; Test 1: Field Configuration Creation
(display "Test 1: Field Configuration Creation\n")
(let* ((field-type 'scalar)
       (field-values '(1.0 2.0 3.0 4.0))
       (coupling 0.1)
       (field (make-field-configuration field-type field-values coupling)))
  (if (and (list? field)
           (>= (length field) 6)
           (eq? (list-ref field 0) 'field-configuration)
           (eq? (list-ref field 2) field-type))
      (display "  ✓ Field configuration creation works\n")
      (begin (display "  ✗ Field configuration creation failed\n") (exit 1))))

;; Test 2: p-adic to Field Theory
(display "Test 2: p-adic to Field Theory\n")
(let* ((padic-valuation '(1 2 3 4))
       (prime 3)
       (depth 4)
       (field (padic-to-field-theory padic-valuation prime depth)))
  (if (and (list? field)
           (eq? (list-ref field 0) 'field-configuration))
      (display "  ✓ p-adic to field theory mapping works\n")
      (begin (display "  ✗ p-adic to field theory mapping failed\n") (exit 1))))

;; Test 3: E8 to Field Theory
(display "Test 3: E8 to Field Theory\n")
(let* ((e8-vector '(e8-vector (uuid) '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0) '()))
       (field (e8-to-field-theory e8-vector 'spectral)))
  (if (and (list? field)
           (eq? (list-ref field 0) 'field-configuration))
      (display "  ✓ E8 to field theory mapping works\n")
      (begin (display "  ✗ E8 to field theory mapping failed\n") (exit 1))))

;; Test 4: Field Evolution
(display "Test 4: Field Evolution\n")
(let* ((field (make-field-configuration 'scalar '(1.0 2.0) 0.1))
       (hamiltonian (lambda (val dt) val))  ; Identity Hamiltonian
       (evolved (evolve-field field hamiltonian 0.1)))
  (if (and (list? evolved)
           (eq? (list-ref evolved 0) 'field-configuration))
      (display "  ✓ Field evolution works\n")
      (begin (display "  ✗ Field evolution failed\n") (exit 1))))

(display "\nAll Quantum Field Theory tests passed!\n")

