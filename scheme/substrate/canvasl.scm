;;; substrate/canvasl.scm --- CanvasL Integration
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; CanvasL integration - native S-expression format means no parsing needed.
;;; CanvasL structures are already S-expressions, so we can manipulate them
;;; directly in Scheme.

;;; Code:

;; Load runtime and binary functions (in real implementation, these would be loaded)

;; CanvasL is already S-expressions, so we work with them directly

(define (canvasl-to-cbs canvasl-data)
  "Convert CanvasL data structure to CBS.
CANVASL-DATA: S-expression representing CanvasL structure"
  (let ((bytes (string->utf8 (object->string canvasl-data)))
        (meta `((encoding . "canvasl")
                (content-type . "canvasl")
                (source . "canvasl"))))
    (make-cbs bytes meta)))

(define (cbs-to-canvasl cbs)
  "Convert CBS back to CanvasL structure.
Returns S-expression."
  (let ((bytes (list-ref cbs 2))
        (encoding (assoc-ref (list-ref cbs 3) 'encoding)))
    (if (equal? encoding "canvasl")
        (let ((str (utf8->string bytes)))
          (read (open-input-string str)))
        (error "CBS is not CanvasL encoded" cbs))))

(define (canvasl-parse-file filename)
  "Parse CanvasL file (JSONL format) into S-expressions.
Returns list of CanvasL objects."
  (let ((port (open-input-file filename))
        (objects '()))
    (do ((line (read-line port) (read-line port)))
        ((eof-object? line))
      (if (and (> (string-length line) 0)
               (not (string-prefix? "@" line))
               (not (string-prefix? ";" line)))
          (let ((obj (json->sexp line)))
            (set! objects (cons obj objects)))))
    (close-port port)
    (reverse objects)))

(define (json->sexp json-str)
  "Convert JSON string to S-expression.
Simple implementation - in production use proper JSON parser."
  ;; Placeholder: would use proper JSON parser
  (read (open-input-string json-str)))

(define (canvasl-write-file filename objects)
  "Write CanvasL objects to file in JSONL format."
  (let ((port (open-output-file filename)))
    (for-each (lambda (obj)
                (display (sexp->json obj) port)
                (newline port))
              objects)
    (close-port port)))

(define (sexp->json sexp)
  "Convert S-expression to JSON string.
Simple implementation - in production use proper JSON encoder."
  ;; Placeholder: would use proper JSON encoder
  (object->string sexp))

;; CanvasL Substrate Operations

(define (canvasl-create-memory canvasl-data)
  "Create memory object from CanvasL data.
Returns (memory-object uri)."
  (let ((cbs (canvasl-to-cbs canvasl-data)))
    (store-memory-object cbs)
    (let ((hash (list-ref cbs 5))
          (uri (content-address hash)))
      (list cbs uri))))

(define (canvasl-get-memory uri)
  "Get CanvasL data from memory object.
Returns CanvasL S-expression."
  (let ((cbs (substrate-get-memory uri)))
    (if cbs
        (cbs-to-canvasl cbs)
        #f)))

;; Helper function
(define (string-prefix? prefix str)
  "Check if string starts with prefix."
  (let ((prefix-len (string-length prefix))
        (str-len (string-length str)))
    (and (>= str-len prefix-len)
         (let loop ((i 0))
           (if (= i prefix-len)
               #t
               (if (char=? (string-ref prefix i) (string-ref str i))
                   (loop (+ i 1))
                   #f))))))

;; Functions are exported by default in R5RS

