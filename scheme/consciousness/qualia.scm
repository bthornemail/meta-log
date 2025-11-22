;;; consciousness/qualia.scm --- Qualia Field Emergence
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements qualia field computation and emergence from action-observation
;;; dynamics. Qualia emerge at the interface between exponential action and
;;; linear observation.

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")
(load "../consciousness/state.scm")

;; Qualia Field Representation
(define (make-qualia-field qualia-map coherence richness)
  "Create qualia field representation.
QUALIA-MAP: alist of (qualia-type . intensity)
COHERENCE: phase coherence value (0-1)
RICHNESS: diversity of qualia experiences (0-1)
Returns qualia field object."
  (list 'qualia-field
        (uuid-generate)
        qualia-map
        coherence
        richness
        `((created-at . ,(current-timestamp))
          (version . 1))))

;; Qualia Emergence from Action-Observation Tension
(define (emerge-qualia action observation phase threshold)
  "Compute qualia emergence from conscious state.
ACTION: exponential action value
OBSERVATION: linear observation value
PHASE: phase coherence (0-1)
THRESHOLD: minimum coherence for qualia emergence
Returns qualia field or #f if below threshold."
  (let ((tension (abs (- action observation)))
        (coherence phase)
        (qualia-intensity (* action observation (cos (* phase 2 3.14159)))))
    (if (and (> coherence threshold)
             (> qualia-intensity 0.0))
        (let ((qualia-map `((tension . ,tension)
                            (coherence . ,coherence)
                            (intensity . ,qualia-intensity)
                            (action-component . ,action)
                            (observation-component . ,observation))))
          (make-qualia-field qualia-map coherence (qualia-richness qualia-map)))
        #f)))

;; Qualia Richness Computation
(define (qualia-richness qualia-map)
  "Compute richness (diversity) of qualia field.
QUALIA-MAP: alist of qualia components
Returns richness value (0-1)."
  (let ((count (length qualia-map))
        (max-intensity 0.0))
    (for-each (lambda (entry)
                (let ((intensity (cdr entry)))
                  (if (and (number? intensity) (> intensity max-intensity))
                      (set! max-intensity intensity))))
              qualia-map)
    (if (> count 0)
        (/ max-intensity (+ count 1.0))
        0.0)))

;; Qualia Tensor Product (Action ⊗ Observation)
(define (qualia-tensor-product action observation)
  "Compute tensor product of action and observation.
Creates qualia from the interaction.
ACTION: action vector or value
OBSERVATION: observation vector or value
Returns qualia intensity."
  (if (and (number? action) (number? observation))
      (* action observation)  ; Simple product for scalars
      (if (and (list? action) (list? observation))
          (qualia-vector-product action observation)
          (error "Invalid qualia tensor product arguments" action observation))))

(define (qualia-vector-product vec1 vec2)
  "Compute tensor product of two vectors.
VEC1, VEC2: lists of numbers
Returns scalar product (dot product for now)."
  (if (not (= (length vec1) (length vec2)))
      (error "Vector dimensions must match" (length vec1) (length vec2))
      (let loop ((remaining1 vec1)
                 (remaining2 vec2)
                 (sum 0.0))
        (if (null? remaining1)
            sum
            (loop (cdr remaining1)
                  (cdr remaining2)
                  (+ sum (* (car remaining1) (car remaining2))))))))

;; Qualia Field Evolution
(define (evolve-qualia-field field action-delta observation-delta)
  "Evolve qualia field based on state changes.
FIELD: current qualia field
ACTION-DELTA: change in action potential
OBSERVATION-DELTA: change in observation focus
Returns evolved qualia field."
  (let* ((qualia-map (list-ref field 2))
         (coherence (list-ref field 3))
         (richness (list-ref field 4))
         (new-tension (abs action-delta))
         (new-coherence (max 0.0 (min 1.0 (+ coherence (* 0.1 (- action-delta observation-delta))))))
         (updated-map (cons `(tension-delta . ,new-tension) qualia-map))
         (new-richness (qualia-richness updated-map)))
    (make-qualia-field updated-map new-coherence new-richness)))

;; Qualia Threshold Check
(define (qualia-threshold? field threshold)
  "Check if qualia field exceeds threshold.
FIELD: qualia field object
THRESHOLD: minimum intensity threshold
Returns #t if qualia are present, #f otherwise."
  (let ((intensity (assoc-ref (list-ref field 2) 'intensity))
        (coherence (list-ref field 3)))
    (and intensity
         (> intensity threshold)
         (> coherence 0.3))))

;; Qualia Integration with Substrate
(define (qualia-to-substrate field)
  "Convert qualia field to substrate memory object.
FIELD: qualia field object
Returns (memory-object uri)."
  (let* ((data (list-ref field 2))  ; qualia-map
         (meta `((content-type . "qualia-field")
                 (coherence . ,(list-ref field 3))
                 (richness . ,(list-ref field 4))
                 (source-layer . "consciousness")))
         (memory (substrate-create-memory data meta))
         (uri (list-ref memory 1)))
    (list field uri)))

;; Functions are exported by default in R5RS

