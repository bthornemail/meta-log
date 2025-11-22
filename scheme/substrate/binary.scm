;;; substrate/binary.scm --- Canonical Binary Substrate (CBS)
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements the Binary Layer Protocol (BLP) with Canonical Binary
;;; Substrate (CBS) format as specified in RFC-MLSP-0001.

;;; Code:

;; Load runtime functions
(load "../substrate/runtime.scm")

;; Helper: list to bytevector
(define (list->bytevector lst)
  "Convert list of integers to bytevector."
  (let ((bv (make-bytevector (length lst))))
    (do ((i 0 (+ i 1))
         (remaining lst (cdr remaining)))
        ((null? remaining) bv)
      (bytevector-u8-set! bv i (car remaining)))))

;; CBS Format
(define (make-cbs bytes meta)
  "Create Canonical Binary Substrate (CBS) object.
BYTES: bytevector or list of bytes
META: alist of metadata"
  (let ((length-bytes (if (bytevector? bytes)
                          (bytevector-length bytes)
                          (length bytes)))
        (encoding (or (assoc-ref meta 'encoding) "raw"))
        (reversible (or (assoc-ref meta 'reversible) #t))
        (hash (content-hash bytes meta)))
    (list 'cbs
          (uuid-generate)
          bytes
          (append meta
                  `((encoding . ,encoding)
                    (length . ,length-bytes)
                    (created-at . ,(current-timestamp))
                    (version . 1)))
          `((exec . "none")
            (max-len . ,length-bytes)
            (reversible . ,reversible))
          hash)))

;; Binary Transformations

(define (binary-xor cbs mask)
  "XOR transform on CBS with mask.
Returns new CBS."
  (let ((bytes (list-ref cbs 2))
        (meta (list-ref cbs 3))
        (mask-bytes (if (bytevector? mask) mask (list->bytevector mask))))
    (if (not (bytevector? bytes))
        (set! bytes (list->bytevector bytes)))
    (let ((result (make-bytevector (bytevector-length bytes))))
      (do ((i 0 (+ i 1)))
          ((= i (bytevector-length bytes)))
        (bytevector-u8-set! result
                            i
                            (logxor (bytevector-u8-ref bytes i)
                                    (bytevector-u8-ref mask-bytes
                                                      (modulo i (bytevector-length mask-bytes))))))
      (make-cbs result
                (append meta
                        `((transform . "xor")
                          (transform-history . ,(cons "xor" (or (assoc-ref meta 'transform-history) '())))))))))

(define (binary-rotate cbs n-bits direction)
  "Rotate bits in CBS.
N-BITS: number of bits to rotate
DIRECTION: 'left or 'right"
  (let ((bytes (list-ref cbs 2))
        (meta (list-ref cbs 3)))
    (if (not (bytevector? bytes))
        (set! bytes (list->bytevector bytes)))
    (let ((result (make-bytevector (bytevector-length bytes)))
          (n-bits-mod (modulo n-bits 8)))  ; Normalize to 0-7 range
      (do ((i 0 (+ i 1)))
          ((= i (bytevector-length bytes)))
        (let ((byte (bytevector-u8-ref bytes i)))
          (bytevector-u8-set! result
                              i
                              (logand 255  ; Mask to 8 bits
                                      (if (eq? direction 'left)
                                          (logior (ash (logand byte 255) n-bits-mod)
                                                  (ash (logand byte 255) (- n-bits-mod 8)))
                                          (logior (ash (logand byte 255) (- n-bits-mod))
                                                  (ash (logand byte 255) (- 8 n-bits-mod))))))))
      (make-cbs result
                (append meta
                        `((transform . "rotate")
                          (transform-history . ,(cons (string-append "rotate-" (symbol->string direction) "-" (number->string n-bits))
                                                      (or (assoc-ref meta 'transform-history) '())))))))))

(define (binary-slice cbs start end)
  "Extract slice from CBS.
Returns new CBS with subset of bytes."
  (let ((bytes (list-ref cbs 2))
        (meta (list-ref cbs 3)))
    (if (not (bytevector? bytes))
        (set! bytes (list->bytevector bytes)))
    (let ((slice-len (- end start))
          (result (make-bytevector (- end start))))
      (do ((i 0 (+ i 1))
           (j start (+ j 1)))
          ((= i slice-len))
        (bytevector-u8-set! result i (bytevector-u8-ref bytes j)))
      (make-cbs result
                (append meta
                        `((transform . "slice")
                          (slice-start . ,start)
                          (slice-end . ,end)
                          (transform-history . ,(cons (string-append "slice-" (number->string start) "-" (number->string end))
                                                      (or (assoc-ref meta 'transform-history) '())))))))))

(define (binary-concat . cbss)
  "Concatenate multiple CBS objects.
Returns new CBS with concatenated bytes."
  (if (null? cbss)
      (error "binary-concat requires at least one CBS"))
  (let ((all-bytes '())
        (all-meta (list-ref (car cbss) 3)))
    (for-each (lambda (cbs)
                (let ((bytes (list-ref cbs 2)))
                  (if (bytevector? bytes)
                      (set! all-bytes (append all-bytes (bytevector->list bytes)))
                      (set! all-bytes (append all-bytes bytes)))))
              cbss)
    (make-cbs (list->bytevector all-bytes)
              (append all-meta
                      `((transform . "concat")
                        (transform-history . ,(cons "concat"
                                                    (or (assoc-ref all-meta 'transform-history) '()))))))))

(define (binary-hash cbs)
  "Compute hash of CBS.
Returns hash string."
  (list-ref cbs 5))

;; Substrate Transform API

(define (substrate-transform input-id operator params)
  "Transform binary substrate.
INPUT-ID: URI or ID of input CBS
OPERATOR: transformation operator name
PARAMS: alist of parameters"
  (let ((input-cbs (substrate-get-memory input-id)))
    (if (not input-cbs)
        (error "Input CBS not found" input-id))
    (let ((result (case (string->symbol operator)
                    ((xor) (binary-xor input-cbs (assoc-ref params 'mask)))
                    ((rotate) (binary-rotate input-cbs
                                            (assoc-ref params 'n-bits)
                                            (assoc-ref params 'direction)))
                    ((slice) (binary-slice input-cbs
                                          (assoc-ref params 'start)
                                          (assoc-ref params 'end)))
                    ((concat) (apply binary-concat (assoc-ref params 'inputs)))
                    (else (error "Unknown operator" operator)))))
      (store-memory-object result)
      (let ((hash (list-ref result 5))
            (uri (content-address hash)))
        (list result uri)))))

;; Functions are exported by default in R5RS

