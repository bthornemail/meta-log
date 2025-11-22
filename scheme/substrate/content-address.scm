;;; substrate/content-address.scm --- Content Addressing (mlss:// URIs)
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements content addressing with mlss:// URI scheme as specified in
;;; RFC-MLSP-0001.

;;; Code:

;; Load runtime functions (in real implementation, these would be loaded)

;; mlss:// URI Scheme

(define (make-mlss-uri algorithm hash)
  "Create mlss:// URI from algorithm and hash.
ALGORITHM: hash algorithm name (e.g., 'sha3-256)
HASH: hash value as hex string"
  (string-append "mlss://" (symbol->string algorithm) "/" hash))

(define (parse-mlss-uri uri)
  "Parse mlss:// URI into (algorithm hash).
Returns (algorithm . hash) or #f if invalid."
  (if (not (and (>= (string-length uri) 7)
                (string=? (substring uri 0 7) "mlss://")))
      #f
      (let ((rest (substring uri 7))
            (slash-pos (string-find-char rest #\/)))
        (if (not slash-pos)
            #f
            (cons (string->symbol (substring rest 0 slash-pos))
                  (substring rest (+ slash-pos 1))))))))

(define (mlss-uri? uri)
  "Check if string is a valid mlss:// URI."
  (and (>= (string-length uri) 7)
       (string=? (substring uri 0 7) "mlss://")
       (parse-mlss-uri uri)))

;; Content Store Interface

(define *content-store-by-hash* (make-hash-table))
(define *content-store-by-id* (make-hash-table))

(define (store-by-content-address obj)
  "Store object by content address.
Returns mlss:// URI."
  (let ((hash (if (list? obj)
                  (list-ref obj 5)  ; hash is 6th element in memory-object/cbs
                  (content-hash obj '())))
        (algorithm 'sha3-256))
    (hash-table-set! *content-store-by-hash* hash obj)
    (make-mlss-uri algorithm hash)))

(define (resolve-content-address uri)
  "Resolve mlss:// URI to object.
Returns object or #f if not found."
  (let ((parsed (parse-mlss-uri uri)))
    (if (not parsed)
        #f
        (let ((hash (cdr parsed)))
          (hash-table-ref *content-store-by-hash* hash #f)))))

;; Content Address Resolution

(define (substrate-resolve-uri uri)
  "Resolve mlss:// URI to memory object.
Returns memory object or #f."
  (resolve-content-address uri))

;; Helper function for string-find-char
(define (string-find-char str char)
  "Find position of character in string. Returns #f if not found."
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond ((= i len) #f)
            ((char=? (string-ref str i) char) i)
            (else (loop (+ i 1)))))))

;; Functions are exported by default in R5RS

