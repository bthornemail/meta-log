;;; test-waveform.scm --- Tests for Waveform Functions
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for waveform computation functions.

;;; Code:

;; Load dependencies
(load "../substrate/waveform.scm")

;; Test helper
(define (test-passed name)
  (display (string-append "Testing " name "... PASS\n")))

(define (test-failed name reason)
  (display (string-append "Testing " name "... FAIL: " reason "\n")))

;; Test waveform creation
(define (test-waveform-create)
  "Test waveform creation."
  (let ((samples '(1 2 3 4 5))
        (sample-rate 48000)
        (duration 1.0)
        (channels 1))
    (let ((waveform (waveform-create samples sample-rate duration channels)))
      (if (and (list? waveform)
               (eq? (car waveform) 'waveform-substrate))
          (test-passed "waveform-create")
          (test-failed "waveform-create" "Invalid waveform structure")))))

;; Test FFT computation
(define (test-waveform-compute-fft)
  "Test waveform FFT computation."
  (let ((samples '(1.0 2.0 3.0 4.0))
        (waveform (waveform-create '(1.0 2.0 3.0 4.0) 48000 1.0 1)))
    (let ((fft-result (waveform-compute-fft waveform)))
      (if (and (list? fft-result)
               (eq? (car fft-result) 'frequency-domain)
               (list? (list-ref fft-result 1)))
          (test-passed "waveform-compute-fft")
          (test-failed "waveform-compute-fft" "Invalid FFT result structure")))))

;; Test p-adic signature
(define (test-waveform-compute-padic-signature)
  "Test p-adic signature computation."
  (let ((waveform (waveform-create '(1 2 3 4 5) 48000 1.0 1))
        (prime 2)
        (depth 5))
    (let ((result (waveform-compute-padic-signature waveform prime depth)))
      (if (and (list? result)
               (eq? (car result) 'padic-signature)
               (list? (list-ref result 1)))
          (test-passed "waveform-compute-padic-signature")
          (test-failed "waveform-compute-padic-signature" "Invalid p-adic signature structure")))))

;; Test E8 signature
(define (test-waveform-compute-e8-signature)
  "Test E8 signature computation."
  (let ((waveform (waveform-create '(1.0 2.0 3.0 4.0) 48000 1.0 1)))
    (let ((result (waveform-compute-e8-signature waveform)))
      (if (and (list? result)
               (eq? (car result) 'e8-signature)
               (list? (list-ref result 1)))
          (test-passed "waveform-compute-e8-signature")
          (test-failed "waveform-compute-e8-signature" "Invalid E8 signature structure")))))

;; Run all tests
(define (test-waveform-all)
  "Run all waveform tests."
  (display "=== Waveform Function Tests ===\n")
  (test-waveform-create)
  (test-waveform-compute-fft)
  (test-waveform-compute-padic-signature)
  (test-waveform-compute-e8-signature)
  (display "=== Tests Complete ===\n"))

;; Run tests if executed directly
(if (not (defined? 'main))
    (test-waveform-all))

