;;; substrate/binary.test.scm --- Tests for Binary Substrate
;;; Meta-Log Substrate System - R5RS Scheme Implementation

;;; Commentary:
;;; Unit tests for binary substrate operations.

;;; Code:

;; Load from parent directory
(load "../substrate/runtime.scm")
(load "../substrate/canvasl.scm")  ; For string-prefix?
(load "../substrate/binary.scm")

;; Test CBS creation
(define (test-make-cbs)
  "Test CBS creation."
  (let ((bytes '(1 2 3 4))
        (meta '((encoding . "raw"))))
    (let ((cbs (make-cbs bytes meta)))
      (and (list? cbs)
           (eq? (car cbs) 'cbs)
           (= (length cbs) 6)))))

;; Test binary XOR
(define (test-binary-xor)
  "Test XOR transformation."
  (let ((bytes '(1 2 3 4))
        (meta '((encoding . "raw")))
        (mask '(255 0 255 0)))
    (let ((cbs (make-cbs bytes meta)))
      (let ((result (binary-xor cbs mask)))
        (and (list? result)
             (eq? (car result) 'cbs))))))

;; Test binary rotate
(define (test-binary-rotate)
  "Test rotation transformation."
  (let ((bytes '(1 2 3 4))
        (meta '((encoding . "raw"))))
    (let ((cbs (make-cbs bytes meta)))
      (let ((result (binary-rotate cbs 3 'left)))
        (and (list? result)
             (eq? (car result) 'cbs))))))

;; Test binary slice
(define (test-binary-slice)
  "Test slice extraction."
  (let ((bytes '(1 2 3 4 5 6 7 8))
        (meta '((encoding . "raw"))))
    (let ((cbs (make-cbs bytes meta)))
      (let ((result (binary-slice cbs 2 5)))
        (and (list? result)
             (eq? (car result) 'cbs))))))

;; Test binary concat
(define (test-binary-concat)
  "Test concatenation."
  (let ((bytes1 '(1 2 3))
        (bytes2 '(4 5 6))
        (meta '((encoding . "raw"))))
    (let ((cbs1 (make-cbs bytes1 meta))
          (cbs2 (make-cbs bytes2 meta)))
      (let ((result (binary-concat cbs1 cbs2)))
        (and (list? result)
             (eq? (car result) 'cbs))))))

;; Run all tests
(define (run-binary-tests)
  "Run all binary tests."
  (display "Testing CBS creation...")
  (if (test-make-cbs)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "Testing binary XOR...")
  (if (test-binary-xor)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "Testing binary rotate...")
  (if (test-binary-rotate)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "Testing binary slice...")
  (if (test-binary-slice)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "Testing binary concat...")
  (if (test-binary-concat)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "\nBinary tests complete.\n"))

;; Run tests if executed directly
(if (not (defined? 'run-tests))
    (run-binary-tests))

