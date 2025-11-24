;;; substrate/waveform.scm --- Waveform Layer Protocol (WLP)
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements the Waveform Layer Protocol (WLP) with dual representation
;;; (time + frequency domain) as specified in RFC-MLSP-0001.

;;; Code:

;; Load runtime and binary functions
(load "../substrate/runtime.scm")
(load "../substrate/binary.scm")

;; Waveform Substrate Format
(define (make-waveform-substrate time-domain frequency-domain padic-signature e8-signature meta)
  "Create waveform substrate object.
TIME-DOMAIN: alist with samples, sample-rate, duration, channels
FREQUENCY-DOMAIN: alist with coefficients, fft-size, computed flag
PADIC-SIGNATURE: alist with prime, depth, valuations, computed flag
E8-SIGNATURE: alist with harmonic-projection, symmetry-class, computed flag
META: alist of metadata"
  (list 'waveform-substrate
        (uuid-generate)
        time-domain
        frequency-domain
        padic-signature
        e8-signature
        meta
        (content-hash (list time-domain frequency-domain) meta)))

;; Time domain operations
(define (waveform-get-samples waveform)
  "Get time-domain samples from waveform."
  (assoc-ref (list-ref waveform 2) 'samples))

(define (waveform-get-sample-rate waveform)
  "Get sample rate from waveform."
  (assoc-ref (list-ref waveform 2) 'sample-rate))

(define (waveform-get-duration waveform)
  "Get duration in seconds from waveform."
  (assoc-ref (list-ref waveform 2) 'duration))

;; Load shared helpers
(load "../common/helpers.scm")

;; Note: Helper functions (take-every-nth, iota, real-to-complex, take, make-list)
;; are now defined in common/helpers.scm

;; Define pi constant (if not available)
(define pi 3.141592653589793)

;; Simple FFT implementation (Cooley-Tukey algorithm)
(define (fft-recursive samples)
  "Recursive FFT implementation (Cooley-Tukey).
SAMPLES: list of complex numbers (or real numbers treated as complex)
Returns list of complex numbers."
  (let ((n (length samples)))
    (if (<= n 1)
        samples
        (let* ((even (fft-recursive (take-every-nth samples 2 0)))  ; Even indices
               (odd (fft-recursive (take-every-nth samples 2 1)))   ; Odd indices
               (twiddle-factors (map (lambda (k)
                                      (let ((angle (* -2.0 pi (/ k n))))
                                        (list (cos angle) (sin angle))))  ; Complex e^(-2πik/n)
                                    (iota n))))
          (append (map (lambda (e o tw)
                        (let ((tw-e-real (- (* (car e) (car tw)) (* (cadr e) (cadr tw))))
                              (tw-e-imag (+ (* (car e) (cadr tw)) (* (cadr e) (car tw)))))
                          (list (+ (car o) tw-e-real) (+ (cadr o) tw-e-imag))))
                      even odd twiddle-factors)
                  (map (lambda (e o tw)
                        (let ((tw-e-real (- (* (car e) (car tw)) (* (cadr e) (cadr tw))))
                              (tw-e-imag (+ (* (car e) (cadr tw)) (* (cadr e) (car tw)))))
                          (list (- (car o) tw-e-real) (- (cadr o) tw-e-imag))))
                      even odd twiddle-factors))))))

;; Helper functions (take-every-nth, iota, real-to-complex) are in common/helpers.scm

;; Frequency domain operations (lazy computation)
(define (waveform-compute-fft waveform)
  "Compute FFT for waveform.
Returns frequency domain representation with coefficients."
  (let ((samples (waveform-get-samples waveform))
        (sample-rate (waveform-get-sample-rate waveform)))
    (if (null? samples)
        (list 'frequency-domain
              '((coefficients . ())
                (fft-size . 0)
                (computed . #t)))
        (let* ((complex-samples (map real-to-complex samples))
               (fft-result (fft-recursive complex-samples))
               (fft-size (length fft-result))
               (magnitudes (map (lambda (c)
                                 (sqrt (+ (* (car c) (car c))
                                          (* (cadr c) (cadr c)))))
                               fft-result)))
          (list 'frequency-domain
                `((coefficients . ,magnitudes)
                  (fft-size . ,fft-size)
                  (computed . #t)))))))

;; p-adic valuation helper (from Q* scoring)
(define (p-adic-valuation n p)
  "Compute p-adic valuation v_p(n)."
  (if (or (zero? n) (< n 0))
      0
      (let loop ((num (abs n))
                 (count 0))
        (if (or (zero? num) (not (zero? (modulo num p))))
            count
            (loop (quotient num p) (+ count 1))))))

;; p-adic signature computation
(define (waveform-compute-padic-signature waveform prime depth)
  "Compute p-adic signature for waveform.
PRIME: prime number for p-adic valuation
DEPTH: maximum depth for valuation computation
Returns p-adic signature with valuations."
  (let ((samples (waveform-get-samples waveform)))
    (if (null? samples)
        (list 'padic-signature
              `((prime . ,prime)
                (depth . ,depth)
                (valuations . ())
                (computed . #t)))
        (let* ((sample-count (min depth (length samples)))
               (sample-subset (take samples sample-count))
               (valuations (map (lambda (s)
                                (let ((sample-int (round (abs s))))
                                  (p-adic-valuation sample-int prime)))
                              sample-subset)))
          (list 'padic-signature
                `((prime . ,prime)
                  (depth . ,depth)
                  (valuations . ,valuations)
                  (computed . #t)))))))

(define (take list n)
  "Take first N elements from list."
  (if (or (null? list) (<= n 0))
      '()
      (cons (car list) (take (cdr list) (- n 1)))))

;; E8 signature computation
(define (waveform-compute-e8-signature waveform)
  "Compute E8 harmonic signature for waveform.
Projects frequency domain coefficients to 8D E8 space.
Returns E8 signature with harmonic projection."
  (let ((freq-domain (waveform-compute-fft waveform))
        (coefficients (assoc-ref (list-ref freq-domain 1) 'coefficients)))
    (if (null? coefficients)
        (list 'e8-signature
              `((harmonic-projection . ,(make-list 8 0.0))
                (symmetry-class . 0)
                (computed . #t)))
        (let* ((coeff-count (length coefficients))
               (coeff-sum (apply + coefficients))
               (coeff-mean (if (zero? coeff-count) 0.0 (/ coeff-sum coeff-count)))
               ;; Project to 8D: use first 8 coefficients or pad/truncate
               (projection (let loop ((coeffs coefficients)
                                     (result '())
                                     (i 0))
                            (if (>= i 8)
                                (reverse result)
                                (if (null? coeffs)
                                    (loop '() (cons 0.0 result) (+ i 1))
                                    (loop (cdr coeffs) (cons (car coeffs) result) (+ i 1))))))
               ;; Normalize projection
               (norm (sqrt (apply + (map (lambda (x) (* x x)) projection))))
               (normalized (if (zero? norm)
                              projection
                              (map (lambda (x) (/ x norm)) projection)))
               ;; Compute symmetry class (simplified: based on coefficient distribution)
               (symmetry-class (if (zero? coeff-count)
                                  0
                                  (let ((variance (/ (apply + (map (lambda (x)
                                                                    (let ((diff (- x coeff-mean)))
                                                                      (* diff diff)))
                                                                  coefficients))
                                                    coeff-count)))
                                    (round (* 10 variance))))))
          (list 'e8-signature
                `((harmonic-projection . ,normalized)
                  (symmetry-class . ,symmetry-class)
                  (computed . #t))))))

(define (make-list n value)
  "Create list of N copies of VALUE."
  (let loop ((i 0) (result '()))
    (if (>= i n)
        (reverse result)
        (loop (+ i 1) (cons value result)))))

;; Waveform creation from samples
(define (waveform-create samples sample-rate duration channels)
  "Create waveform from time-domain samples."
  (let ((time-domain `((samples . ,samples)
                      (sample-rate . ,sample-rate)
                      (duration . ,duration)
                      (channels . ,channels)))
        (meta `((source . "synthesized")
                (created-at . ,(current-timestamp)))))
    (make-waveform-substrate time-domain
                            '((computed . #f))  ; frequency domain not computed yet
                            #f  ; p-adic not computed
                            #f  ; E8 not computed
                            meta)))

;; Substrate Waveform API

(define (substrate-waveform-create samples sample-rate duration channels)
  "Create waveform substrate from samples.
Returns (waveform-object uri)."
  (let ((waveform (waveform-create samples sample-rate duration channels)))
    (store-memory-object waveform)
    (let ((hash (list-ref waveform 7))
          (uri (content-address hash)))
      (list waveform uri))))

;; Convenience wrapper for simple waveform creation
(define (make-waveform samples meta sample-rate)
  "Create waveform from samples (convenience wrapper).
SAMPLES: list of sample values
META: metadata alist (can be empty)
SAMPLE-RATE: sample rate in Hz
Returns waveform substrate object."
  (let* ((time-domain `((samples . ,samples)
                        (sample-rate . ,sample-rate)
                        (duration . ,(if (null? samples)
                                        0.0
                                        (/ (length samples) sample-rate)))
                        (channels . 1)))
         (frequency-domain '((computed . #f)))
         (padic-signature '((computed . #f)))
         (e8-signature '((computed . #f))))
    (make-waveform-substrate time-domain frequency-domain padic-signature e8-signature meta)))

;; Functions are exported by default in R5RS

