;;; substrate/runtime.test.scm --- Tests for Substrate Runtime
;;; Meta-Log Substrate System - R5RS Scheme Implementation

;;; Commentary:
;;; Unit tests for substrate runtime operations.

;;; Code:

;; Load from parent directory
(load "../substrate/runtime.scm")
(load "../substrate/canvasl.scm")  ; For string-prefix?

;; Test UUID generation
(define (test-uuid-generate)
  "Test UUID generation."
  (let ((uuid1 (uuid-generate))
        (uuid2 (uuid-generate)))
    (and (string? uuid1)
         (string? uuid2)
         (> (string-length uuid1) 0)
         (not (equal? uuid1 uuid2)))))  ; Should be unique

;; Test content hash
(define (test-content-hash)
  "Test content hashing."
  (let ((data '(1 2 3 4))
        (meta '((encoding . "raw"))))
    (let ((hash1 (content-hash data meta))
          (hash2 (content-hash data meta)))
      (and (string? hash1)
           (string? hash2)
           (equal? hash1 hash2)))))  ; Deterministic

;; Test memory object creation
(define (test-make-memory-object)
  "Test memory object creation."
  (let ((data '(1 2 3 4))
        (meta '((encoding . "raw")))
        (constraints '((exec . "none"))))
    (let ((obj (make-memory-object data meta constraints)))
      (and (list? obj)
           (eq? (car obj) 'memory-object)
           (= (length obj) 6)))))

;; Test content addressing
(define (test-content-address)
  "Test content address creation."
  (let ((hash "abc123def456"))
    (let ((uri (content-address hash)))
      (and (string? uri)
           (string-prefix? "mlss://sha3-256/" uri)))))

;; Test substrate-create-memory
(define (test-substrate-create-memory)
  "Test substrate memory creation API."
  (let ((data '(1 2 3 4))
        (meta '((encoding . "raw"))))
    (let ((result (substrate-create-memory data meta)))
      (and (list? result)
           (= (length result) 2)
           (list? (car result))
           (string? (cadr result))
           (string-prefix? "mlss://" (cadr result))))))

;; Run all tests
(define (run-runtime-tests)
  "Run all runtime tests."
  (display "Testing UUID generation...")
  (if (test-uuid-generate)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "Testing content hash...")
  (if (test-content-hash)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "Testing memory object creation...")
  (if (test-make-memory-object)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "Testing content addressing...")
  (if (test-content-address)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "Testing substrate-create-memory...")
  (if (test-substrate-create-memory)
      (display " PASS\n")
      (display " FAIL\n"))
  
  (display "\nRuntime tests complete.\n"))

;; Run tests if executed directly
(if (not (defined? 'run-tests))
    (run-runtime-tests))

