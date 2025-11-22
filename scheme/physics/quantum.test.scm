;;; physics/quantum.test.scm --- Quantum Physics Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for quantum state representation

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../substrate/binary.scm")
(load "../physics/quantum.scm")

;; Test 1: Quantum State Creation
(display "Test 1: Quantum State Creation\n")
(let* ((qubits 2)
       (wavefunction '(0.707 0.707 0.0 0.0))  ; Bell state approximation
       (state (make-quantum-state qubits wavefunction)))
  (if (and (list? state)
           (>= (length state) 4)
           (eq? (list-ref state 0) 'quantum-state)
           (= (list-ref state 1) qubits))
      (display "  ✓ Quantum state creation works\n")
      (begin (display "  ✗ Quantum state creation failed\n") (exit 1))))

;; Test 2: Probability Computation
(display "Test 2: Probability Computation\n")
(let* ((wavefunction '(0.6 0.8 0.0 0.0))
       (probabilities (quantum-compute-probabilities wavefunction)))
  (if (and (list? probabilities)
           (> (length probabilities) 0)
           (number? (car probabilities)))
      (display "  ✓ Probability computation works\n")
      (begin (display "  ✗ Probability computation failed\n") (exit 1))))

;; Test 3: Quantum → CBS Encoding
(display "Test 3: Quantum → CBS Encoding\n")
(let* ((qubits 1)
       (wavefunction '(0.707 0.707))
       (quantum (make-quantum-state qubits wavefunction))
       (cbs (quantum-to-cbs quantum)))
  (if (and (list? cbs)
           (eq? (list-ref cbs 0) 'cbs))
      (display "  ✓ Quantum to CBS encoding works\n")
      (begin (display "  ✗ Quantum to CBS encoding failed\n") (exit 1))))

(display "\nAll quantum physics tests passed!\n")

