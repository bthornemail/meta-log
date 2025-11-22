;;; substrate/provenance.scm --- Provenance Chain Protocol (PCP)
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements the Provenance Chain Protocol (PCP) with hash-linked chains
;;; and Merkle DAGs as specified in RFC-MLSP-0001.

;;; Code:

;; Load runtime functions (in real implementation, these would be loaded)

;; Provenance Record Format
(define (make-provenance-record operation inputs outputs previous-hash cost-metrics)
  "Create a provenance record.
OPERATION: alist with type, operator, params
INPUTS: list of (id hash role) tuples
OUTPUTS: list of (id hash role) tuples
PREVIOUS-HASH: hash of previous record (or #f for genesis)
COST-METRICS: alist of cost metrics"
  (let ((record-id (uuid-generate))
        (timestamp (current-timestamp))
        (record (list 'provenance-record
                      record-id
                      timestamp
                      operation
                      inputs
                      outputs
                      cost-metrics
                      previous-hash)))
    (let ((record-hash (content-hash record '())))
      (list record record-hash))))

;; Provenance Chain Storage
(define *provenance-chain* '())

(define (add-provenance-record record hash)
  "Add provenance record to chain."
  (set! *provenance-chain* (cons (list record hash) *provenance-chain*))
  hash)

;; Get provenance by hash
(define (get-provenance-by-hash hash)
  "Get provenance record by hash."
  (assoc-ref (map (lambda (entry) (cons (list-ref entry 1) (list-ref entry 0)))
                  *provenance-chain*)
             hash))

;; Get provenance chain for a memory object
(define (get-provenance-chain memory-uri)
  "Get full provenance chain for a memory object.
Returns list of provenance records in chronological order."
  (let ((memory-obj (substrate-get-memory memory-uri)))
    (if (not memory-obj)
        '()
        (let ((hash (list-ref memory-obj 5)))
          (build-provenance-chain hash '())))))

(define (build-provenance-chain current-hash chain)
  "Build provenance chain backwards from current hash."
  (let ((record (get-provenance-record-by-output-hash current-hash)))
    (if (not record)
        chain
        (let ((prev-hash (list-ref record 7)))  ; previous-hash
          (if (or (not prev-hash) (member prev-hash chain))  ; prevent cycles
              (cons record chain)
              (build-provenance-chain prev-hash (cons record chain)))))))

(define (get-provenance-record-by-output-hash hash)
  "Find provenance record that produced this hash as output."
  (let loop ((chain *provenance-chain*))
    (if (null? chain)
        #f
        (let ((entry (car chain))
              (record (list-ref entry 0))
              (outputs (list-ref record 5)))
          (if (any-output-matches? outputs hash)
              entry
              (loop (cdr chain)))))))

(define (any-output-matches? outputs hash)
  "Check if any output matches hash."
  (if (null? outputs)
      #f
      (or (equal? (list-ref (car outputs) 1) hash)
          (any-output-matches? (cdr outputs) hash))))

;; Verify provenance chain
(define (verify-provenance-chain chain)
  "Verify integrity of provenance chain.
Returns (valid? errors)."
  (let ((errors '()))
    (do ((i 0 (+ i 1))
         (prev-hash #f))
        ((= i (length chain))
         (list (null? errors) errors))
      (let ((entry (list-ref chain i))
            (record (list-ref entry 0))
            (hash (list-ref entry 1)))
        ;; Verify hash matches record
        (let ((computed-hash (content-hash record '())))
          (if (not (equal? computed-hash hash))
              (set! errors (cons (string-append "Hash mismatch at index " (number->string i)) errors))))
        ;; Verify previous hash link
        (let ((prev-hash-in-record (list-ref record 7)))
          (if (and prev-hash (not (equal? prev-hash prev-hash-in-record)))
              (set! errors (cons (string-append "Previous hash mismatch at index " (number->string i)) errors)))
          (set! prev-hash hash))))))

;; Create provenance for transformation
(define (record-transformation operation input-uris output-uris cost-metrics)
  "Record a transformation in provenance chain.
OPERATION: alist with type, operator, params
INPUT-URIS: list of input memory URIs
OUTPUT-URIS: list of output memory URIs
COST-METRICS: alist of cost metrics"
  (let ((previous-hash (if (null? *provenance-chain*)
                           #f
                           (list-ref (car *provenance-chain*) 1)))
        (inputs (map (lambda (uri)
                       (let ((obj (substrate-get-memory uri)))
                         (if obj
                             (list uri (list-ref obj 5) 'primary)
                             (error "Input memory not found" uri))))
                     input-uris))
        (outputs (map (lambda (uri)
                        (let ((obj (substrate-get-memory uri)))
                          (if obj
                              (list uri (list-ref obj 5) 'primary)
                              (error "Output memory not found" uri))))
                      output-uris)))
    (let* ((record-hash (make-provenance-record operation inputs outputs previous-hash cost-metrics))
           (record (list-ref record-hash 0))
           (hash (list-ref record-hash 1)))
      (add-provenance-record record hash)
      hash)))

;; Substrate Provenance API

(define (substrate-get-provenance uri-or-hash)
  "Get provenance information.
URI-OR-HASH: memory URI or provenance hash"
  (if (and (>= (string-length uri-or-hash) 7)
           (string=? (substring uri-or-hash 0 7) "mlss://"))
      (get-provenance-chain uri-or-hash)
      (let ((record (get-provenance-by-hash uri-or-hash)))
        (if record
            (list record)
            '()))))

(define (substrate-verify-provenance chain)
  "Verify provenance chain integrity."
  (verify-provenance-chain chain))

;; Functions are exported by default in R5RS

