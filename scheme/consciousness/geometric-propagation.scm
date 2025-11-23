;;; consciousness/geometric-propagation.scm --- Geometric Propagation
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements geometric propagation theory from 12-Geometry-of-Mind.md:
;;; - Forward propagation: Point → Edge → Face → Volume (exponential O(2^d))
;;; - Backward propagation: Volume → Face → Edge → Point (exponential compression)
;;; - Parallel observation: Direct Hopf projection (linear O(k))

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")
(load "state.scm")

;; Helper: Check if point is in list
(define (point-in-list? point point-list)
  "Check if point is in point-list (using approximate equality)."
  (let loop ((remaining point-list))
    (if (null? remaining)
        #f
        (if (and (list? (car remaining))
                 (list? point)
                 (= (length (car remaining)) (length point))
                 (let ((diff (map (lambda (a b) (abs (- a b))) (car remaining) point)))
                   (< (apply + diff) 0.0001)))  ; Approximate equality
            #t
            (loop (cdr remaining))))))

;; Helper: Add point to list if not already present
(define (add-unique-point point point-list)
  "Add point to list if not already present."
  (if (point-in-list? point point-list)
      point-list
      (cons point point-list)))

;; Forward Propagation: Exponential Expansion
;; Point → Edge → Face → Volume (O(2^d) growth)

(define (geometric-forward-propagation input-point max-depth)
  "Forward propagation: exponential expansion through dimensional ascent.
INPUT-POINT: starting point (list of coordinates)
MAX-DEPTH: maximum dimensional depth (typically 3: 1D→2D→3D)
Returns list of points representing exponential expansion.
Complexity: O(2^d) where d = max-depth"
  (let* ((current-layer (list input-point))
         (all-points '()))
    ;; Iterate through dimensional layers
    (do ((d 1 (+ d 1)))
        ((> d max-depth) (reverse all-points))
      (let ((next-layer '()))
        ;; For each point in current layer, generate extensions
        (for-each
         (lambda (point)
           (let ((extensions (extend-to-higher-dimension point d)))
             (for-each
              (lambda (ext)
                (set! next-layer (add-unique-point ext next-layer)))
              extensions)))
         current-layer)
        ;; Add current layer to all points
        (set! all-points (append current-layer all-points))
        (set! current-layer next-layer)))))

(define (extend-to-higher-dimension point dimension)
  "Extend point to higher dimension by generating geometric extensions.
POINT: current point (list of coordinates)
DIMENSION: target dimension (1=edge, 2=face, 3=volume)
Returns list of extended points."
  (let ((point-dim (length point))
        (extensions '()))
    ;; Generate extensions along each coordinate axis
    (do ((i 0 (+ i 1)))
        ((>= i point-dim))
      ;; Positive extension
      (let ((ext-pos (append (take point i)
                             (list (+ (list-ref point i) 1.0))
                             (drop point (+ i 1)))))
        (set! extensions (cons ext-pos extensions)))
      ;; Negative extension
      (let ((ext-neg (append (take point i)
                             (list (- (list-ref point i) 1.0))
                             (drop point (+ i 1)))))
        (set! extensions (cons ext-neg extensions))))
    ;; If dimension is higher, pad with zeros
    (if (< point-dim dimension)
        (let ((padded-extensions '()))
          (for-each
           (lambda (ext)
             (let ((padded (append ext (make-list (- dimension point-dim) 0.0))))
               (set! padded-extensions (cons padded padded-extensions))))
           extensions)
          (reverse padded-extensions))
        (reverse extensions))))

;; Helper: Take first n elements
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;; Helper: Drop first n elements
(define (drop lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))

;; Helper: Make list of n elements
(define (make-list n val)
  (if (<= n 0)
      '()
      (cons val (make-list (- n 1) val))))

;; Backward Propagation: Exponential Compression
;; Volume → Face → Edge → Point (O(2^d) compression)

(define (geometric-backward-propagation volume-cells)
  "Backward propagation: exponential compression through dimensional descent.
VOLUME-CELLS: list of points representing volume (3D cells)
Returns single point after compression.
Complexity: O(2^d) compression"
  (if (null? volume-cells)
      '()
      (let ((current-layer volume-cells))
        ;; Iterate from 3D down to 0D
        (do ((d 3 (- d 1)))
            ((<= d 0)
             ;; Return centroid of remaining points
             (if (null? current-layer)
                 '()
                 (compute-centroid current-layer)))
          (let ((next-layer '()))
            ;; Project each cell to lower dimension
            (for-each
             (lambda (cell)
               (let ((projection (project-to-lower-dimension cell d)))
                 (if (not (null? projection))
                     (set! next-layer (add-unique-point projection next-layer)))))
             current-layer)
            (set! current-layer next-layer))))))

(define (project-to-lower-dimension cell dimension)
  "Project cell to lower dimension.
CELL: point in higher dimension
DIMENSION: target dimension
Returns projected point."
  (if (or (null? cell) (< (length cell) dimension))
      cell
      (take cell dimension)))

(define (compute-centroid points)
  "Compute centroid of list of points."
  (if (null? points)
      '()
      (let ((n (length points))
            (dim (length (car points))))
        (let ((sums (make-list dim 0.0)))
          (for-each
           (lambda (point)
             (set! sums (map + sums point)))
           points)
          (map (lambda (s) (/ s n)) sums)))))

;; Parallel Observation: Linear Processing via Hopf Fibrations
;; O(k) complexity where k = number of fibers

(define (geometric-parallel-observation points target-dim fiber-list)
  "Parallel observation: linear processing via Hopf fibrations.
POINTS: list of points to observe
TARGET-DIM: target dimension for observation
FIBER-LIST: list of fiber types ('complex, 'quaternionic, 'octonionic)
Returns list of observed points.
Complexity: O(k) where k = length of fiber-list"
  (let ((observed '()))
    ;; Process each point through each fiber in parallel
    (for-each
     (lambda (point)
       (for-each
        (lambda (fiber-type)
          (let ((observation (hopf-project-point point fiber-type target-dim)))
            (if (not (null? observation))
                (set! observed (add-unique-point observation observed)))))
        fiber-list))
     points)
    (reverse observed)))

(define (hopf-project-point point fiber-type target-dim)
  "Project point through Hopf fiber.
POINT: input point
FIBER-TYPE: 'complex, 'quaternionic, or 'octonionic
TARGET-DIM: target dimension
Returns projected point."
  (let ((point-dim (length point)))
    (cond
     ((eq? fiber-type 'complex)
      ;; Complex Hopf: S³ → S² (reduce 3D to 2D)
      (if (>= point-dim 3)
          (take point 2)
          point))
     ((eq? fiber-type 'quaternionic)
      ;; Quaternionic Hopf: S⁷ → S⁴ (reduce 7D to 4D)
      (if (>= point-dim 7)
          (take point 4)
          (if (>= point-dim 4)
              point
              (append point (make-list (- 4 point-dim) 0.0)))))
     ((eq? fiber-type 'octonionic)
      ;; Octonionic Hopf: S¹⁵ → S⁸ (reduce 15D to 8D)
      (if (>= point-dim 15)
          (take point 8)
          (if (>= point-dim 8)
              point
              (append point (make-list (- 8 point-dim) 0.0)))))
     (else
      ;; Default: simple projection to target-dim
      (if (>= point-dim target-dim)
          (take point target-dim)
          (append point (make-list (- target-dim point-dim) 0.0)))))))

;; Integration with Consciousness State

(define (consciousness-geometric-cycle sensory-input)
  "Complete consciousness cycle with geometric propagation.
SENSORY-INPUT: initial sensory input point
Returns (action observation qualia) tuple."
  ;; Forward: Exponential expansion of possibilities
  (let* ((possible-states (geometric-forward-propagation sensory-input 3))
         ;; Parallel observation: Linear processing of environment
         (environmental-obs (geometric-parallel-observation
                            (list sensory-input)
                            2
                            '(complex quaternionic)))
         ;; Backward: Exponential compression to decision
         (chosen-state (geometric-backward-propagation possible-states))
         ;; Qualia emerges at intersection
         (qualia (compute-qualia-intersection chosen-state environmental-obs)))
    (list chosen-state environmental-obs qualia)))

(define (compute-qualia-intersection action-state observation-data)
  "Compute qualia as intersection of action and observation.
ACTION-STATE: chosen state from backward propagation
OBSERVATION-DATA: list of observations from parallel processing
Returns qualia intensity value."
  (if (or (null? action-state) (null? observation-data))
      0.0
      (let ((action-magnitude (compute-magnitude action-state))
            (obs-magnitude (if (null? observation-data)
                               0.0
                               (apply + (map compute-magnitude observation-data)))))
        (* action-magnitude obs-magnitude))))

(define (compute-magnitude point)
  "Compute magnitude (L2 norm) of point."
  (if (null? point)
      0.0
      (sqrt (apply + (map (lambda (x) (* x x)) point)))))

;; Functions are exported by default in R5RS

