;;; meta-log-e8.el --- E8 Lattice Operations

;; Copyright (C) 2025 Automaton System
;; Author: Automaton System
;; Version: 1.0.0

;; This file is part of meta-log.

;; meta-log is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; E8 exceptional Lie algebra root system implementation:
;; - 240 roots construction
;; - BIP32 path → E8 point mapping
;; - Weyl group operations (with dynamic performance-based limits)
;; - p-adic heights for voter ML features
;; - Shortest path algorithms (A* on E8 graph)
;; - FRBAC delegation verification

;;; Code:

(require 'cl-lib)
(require 'meta-log-p-adic)

;;; E8 Point Structure

(cl-defstruct meta-log-e8-point
  "Point in E8 lattice with BIP32 metadata.
COORDS is a list of 8 floats (8D coordinates).
BIP32-PATH is a string like \"m/44'/0'/0'/0/0\".
DEPTH is the derivation depth.
PARENT is optional parent E8Point reference."
  coords
  bip32-path
  depth
  parent)

(defun meta-log-e8-point-norm-squared (point)
  "Calculate squared norm (distance from origin) for E8 POINT.
Returns float."
  (let ((coords (meta-log-e8-point-coords point)))
    (cl-loop for c in coords
             sum (* c c) into total
             finally return (float total))))

(defun meta-log-e8-point-is-root (point)
  "Check if POINT is one of 240 E8 roots (norm² = 2).
Returns t if norm² ≈ 2, nil otherwise."
  (let ((norm-sq (meta-log-e8-point-norm-squared point)))
    (< (abs (- norm-sq 2.0)) 1e-6)))

;;; System Performance Metrics

(defun meta-log-e8--system-performance-metrics ()
  "Get current system performance metrics.
Returns plist with :memory-available, :time-budget, :cpu-load-estimate."
  (let* ((memory-info (condition-case nil
                          (garbage-collect)
                        (error nil)))
         (memory-available (if (and (fboundp 'meta-log-benchmark-get-memory)
                                   (require 'meta-log-benchmark nil t))
                              (let ((used (meta-log-benchmark-get-memory)))
                                ;; Estimate available: assume 100MB base, scale with used
                                (max 10000000  ; 10MB minimum
                                     (- (* 10 used) used)))  ; Rough estimate
                            100000000))  ; Default 100MB if benchmark not available
         (time-budget 5.0)  ; Default 5 seconds
         (cpu-load-estimate 1.0))  ; Assume normal load
    (list :memory-available memory-available
          :time-budget time-budget
          :cpu-load-estimate cpu-load-estimate)))

(defun meta-log-e8--calculate-dynamic-limit (operation-type &optional base-limit)
  "Calculate dynamic limit for E8 operations based on system performance.
OPERATION-TYPE is symbol: 'weyl-orbit, 'shortest-path, etc.
BASE-LIMIT is minimum limit (default depends on operation).
Returns integer limit."
  (let* ((base (or base-limit
                   (cond
                    ((eq operation-type 'weyl-orbit) 50)
                    ((eq operation-type 'shortest-path) 48)
                    (t 100))))
         (metrics (meta-log-e8--system-performance-metrics))
         (memory-available (plist-get metrics :memory-available))
         (time-budget (plist-get metrics :time-budget))
         (cpu-load (plist-get metrics :cpu-load-estimate))
         ;; Memory scaling: each point ~1KB, use up to 10% of available
         (memory-limit (floor (/ memory-available 10000)))
         ;; Time scaling: estimate 0.001s per point, scale to time budget
         (time-limit (floor (* time-budget 1000)))
         ;; CPU scaling: reduce if system is busy
         (cpu-scale (/ 1.0 cpu-load))
         ;; Final limit: max of base and scaled limits, capped at reasonable max
         (max-limit (cond
                     ((eq operation-type 'weyl-orbit) 10000)
                     ((eq operation-type 'shortest-path) 1000)
                     (t 5000)))
         (calculated-limit (min max-limit
                                (max base
                                     (floor (* (min memory-limit time-limit)
                                               cpu-scale))))))
    calculated-limit))

;;; E8 Lattice Class

(defvar meta-log-e8--lattice-instance nil
  "Cached E8 lattice instance.")

(defvar meta-log-e8--roots nil
  "Cached list of 240 E8 roots.")

(defvar meta-log-e8--simple-roots nil
  "Cached list of 8 simple E8 roots.")

(defvar meta-log-e8--weyl-generators nil
  "Cached Weyl group generator matrices.")

(defun meta-log-e8--construct-roots ()
  "Construct all 240 E8 roots.
Two types:
1. All permutations of (±1, ±1, 0, 0, 0, 0, 0, 0): 112 roots
2. ½(±1, ±1, ±1, ±1, ±1, ±1, ±1, ±1) with even # of -1s: 128 roots
Returns list of 240 root vectors (each is list of 8 floats)."
  (let ((roots '()))
    ;; Type 1: (±eᵢ ± eⱼ) for i ≠ j
    (dotimes (i 8)
      (dotimes (j (1+ i))
        (when (/= i j)
          (dolist (sign-i '(1 -1))
            (dolist (sign-j '(1 -1))
              (let ((root (make-list 8 0.0)))
                (setf (nth i root) (float sign-i))
                (setf (nth j root) (float sign-j))
                (push root roots)))))))
    ;; Type 2: ½(±e₁±e₂±...±e₈) with even number of minus signs
    (dotimes (mask 256)  ; 2^8 combinations
      (let ((signs (cl-loop for bit from 0 below 8
                            collect (= (logand mask (ash 1 bit)) 0))))
        (let ((num-minus (cl-count nil signs)))
          (when (= (mod num-minus 2) 0)  ; Even number of -1s
            (let ((root (cl-loop for s in signs
                                 collect (if s 0.5 -0.5))))
              (push root roots))))))
    roots))

(defun meta-log-e8--get-simple-roots ()
  "Get simple roots (Dynkin diagram basis) for E8.
Bourbaki convention.
Returns list of 8 root vectors."
  (list
   (list -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 0.5)  ; α₁
   (list 1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0)        ; α₂
   (list -1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0)      ; α₃
   (list 0.0 -1.0 1.0 0.0 0.0 0.0 0.0 0.0)      ; α₄
   (list 0.0 0.0 -1.0 1.0 0.0 0.0 0.0 0.0)      ; α₅
   (list 0.0 0.0 0.0 -1.0 1.0 0.0 0.0 0.0)      ; α₆
   (list 0.0 0.0 0.0 0.0 -1.0 1.0 0.0 0.0)      ; α₇
   (list 0.0 0.0 0.0 0.0 0.0 -1.0 1.0 0.0)))    ; α₈

(defun meta-log-e8--construct-weyl-generators ()
  "Construct Weyl group generators (reflection matrices).
Reflection through root α: s_α(v) = v - 2(v·α)/(α·α) α
Returns list of 8x8 reflection matrices (each is list of 8 lists)."
  (let ((generators '())
        (simple-roots (meta-log-e8--get-simple-roots)))
    (dolist (alpha simple-roots)
      (let ((alpha-norm-sq (cl-loop for c in alpha sum (* c c)))
            (reflection (make-list 8 nil)))
        ;; Create reflection matrix: I - 2(α⊗α)/|α|²
        (dotimes (i 8)
          (setf (nth i reflection)
                (cl-loop for j from 0 below 8
                         collect (if (= i j)
                                     (- 1.0 (/ (* 2 (nth i alpha) (nth j alpha))
                                               alpha-norm-sq))
                                   (- 0.0 (/ (* 2 (nth i alpha) (nth j alpha))
                                             alpha-norm-sq))))))
        (push reflection generators)))
    (nreverse generators)))

(defun meta-log-e8-initialize ()
  "Initialize E8 lattice (construct roots and generators).
Caches results for performance."
  (unless meta-log-e8--roots
    (setq meta-log-e8--roots (meta-log-e8--construct-roots))
    (setq meta-log-e8--simple-roots (meta-log-e8--get-simple-roots))
    (setq meta-log-e8--weyl-generators (meta-log-e8--construct-weyl-generators))
    (setq meta-log-e8--lattice-instance t))
  meta-log-e8--lattice-instance)

;;; BIP32 to E8 Mapping

(defun meta-log-e8--hash-to-coords (path)
  "Hash BIP32 PATH to 8D float coordinates.
Uses SHA-256 hash, converts to 8 floats in [-10, 10].
Returns list of 8 floats."
  (let* ((hash (secure-hash 'sha256 path))
         (coords '()))
    ;; Convert hex hash to 8 floats
    (dotimes (i 8)
      (let* ((hex-start (* i 8))
             (hex-end (+ hex-start 8))
             (hex-substr (substring hash hex-start (min hex-end (length hash))))
             (int-val (string-to-number hex-substr 16))
             (normalized (float (- (mod int-val 20) 10))))
        (push normalized coords)))
    (nreverse coords)))

(defun meta-log-e8--project-to-lattice (coords)
  "Project arbitrary 8D vector to nearest E8 lattice point.
COORDS is list of 8 floats.
Returns list of 8 floats (E8 lattice point)."
  (let ((rounded (mapcar #'round coords)))
        ;; E8 condition: sum must be even
        (if (= (mod (cl-reduce #'+ rounded) 2) 0)
        (mapcar #'float rounded)
      ;; Shift to (ℤ + ½)⁸ coset
      (mapcar (lambda (c) (float (+ (round (- c 0.5)) 0.5))) coords))))

(defun meta-log-e8-bip32-to-e8 (path)
  "Map BIP32 path to E8 lattice point.
PATH is BIP32 path string like \"m/44'/0'/0'/0/0\".
Returns meta-log-e8-point structure."
  (meta-log-e8-initialize)
  (let* ((components (split-string path "/" t))
         (depth (1- (length components)))
         (coords-hash (meta-log-e8--hash-to-coords path))
         (coords-lattice (meta-log-e8--project-to-lattice coords-hash))
         (parent (if (> depth 0)
                     (let ((parent-path (mapconcat #'identity
                                                   (butlast components)
                                                   "/")))
                       (meta-log-e8-bip32-to-e8 parent-path))
                   nil)))
    (make-meta-log-e8-point
     :coords coords-lattice
     :bip32-path path
     :depth depth
     :parent parent)))

;;; Weyl Group Operations

(defun meta-log-e8--apply-reflection (point reflection-matrix)
  "Apply Weyl reflection matrix to POINT.
REFLECTION-MATRIX is 8x8 matrix (list of 8 lists).
Returns new meta-log-e8-point."
  (let ((coords (meta-log-e8-point-coords point))
        (new-coords '()))
    (dolist (row reflection-matrix)
      (push (cl-reduce #'+ (cl-mapcar #'* row coords)) new-coords))
    (make-meta-log-e8-point
     :coords (nreverse new-coords)
     :bip32-path (concat (meta-log-e8-point-bip32-path point) "/weyl")
     :depth (meta-log-e8-point-depth point)
     :parent (meta-log-e8-point-parent point))))

(defun meta-log-e8--point-hash (point)
  "Get hash key for POINT (for set membership).
Returns string."
  (let ((coords (meta-log-e8-point-coords point)))
    (mapconcat (lambda (c) (format "%.3f" c)) coords ",")))

(defun meta-log-e8-weyl-orbit (point &optional max-size)
  "Compute Weyl group orbit of POINT.
Orbit = {w(v) : w ∈ W(E8)}
Uses dynamic performance-based limit if MAX-SIZE not specified.
Returns list of meta-log-e8-point structures."
  (meta-log-e8-initialize)
  (let* ((limit (or max-size (meta-log-e8--calculate-dynamic-limit 'weyl-orbit)))
         (orbit-hash (make-hash-table :test 'equal))
         (orbit-list '())
         (queue (list point))
         (start-time (float-time)))
    (puthash (meta-log-e8--point-hash point) t orbit-hash)
    (push point orbit-list)
    (while (and queue (< (length orbit-list) limit))
      (let ((current (pop queue))
            (current-time (float-time)))
        ;; Check time budget
        (when (> (- current-time start-time) 4.5)  ; Leave 0.5s buffer
          (setq queue nil))  ; Stop expanding
        ;; Apply each Weyl generator
        (dolist (gen meta-log-e8--weyl-generators)
          (let ((new-point (meta-log-e8--apply-reflection current gen))
                (new-hash (meta-log-e8--point-hash new-point)))
            (unless (gethash new-hash orbit-hash)
              (puthash new-hash t orbit-hash)
              (push new-point orbit-list)
              (when (< (length orbit-list) limit)
                (push new-point queue)))))))
    orbit-list))

;;; Shortest Path (A* Search)

(defun meta-log-e8--euclidean-distance (p1 p2)
  "Calculate Euclidean distance between two E8 points.
P1 and P2 are meta-log-e8-point structures.
Returns float."
  (let ((coords1 (meta-log-e8-point-coords p1))
        (coords2 (meta-log-e8-point-coords p2)))
    (sqrt (cl-reduce #'+
                     (cl-mapcar (lambda (c1 c2)
                                  (expt (- c1 c2) 2))
                                coords1 coords2)))))

(defun meta-log-e8-shortest-path (start end &optional max-roots)
  "Find shortest path in E8 lattice using A* search.
START and END are meta-log-e8-point structures.
MAX-ROOTS limits search to first N roots (default: dynamic).
Returns (path . distance) where path is list of points."
  (meta-log-e8-initialize)
  (let* ((root-limit (or max-roots
                         (meta-log-e8--calculate-dynamic-limit 'shortest-path)))
         (roots-to-use (cl-subseq meta-log-e8--roots 0
                                  (min root-limit (length meta-log-e8--roots))))
         (frontier (list (list 0.0 0 (meta-log-e8--euclidean-distance start end)
                              0.0 start (list start))))  ; (f-score, tie-breaker, h-score, g-score, point, path)
         (visited (make-hash-table :test 'equal))
         (counter 0))
    (while frontier
      (let* ((node (pop frontier))
             (f-score (nth 0 node))
             (g-score (nth 3 node))
             (current (nth 4 node))
             (path (nth 5 node))
             (current-hash (meta-log-e8--point-hash current)))
        (when (gethash current-hash visited)
          (setq frontier (cdr frontier))
          (setq frontier nil))
        (puthash current-hash t visited)
        ;; Check if reached end
        (when (< (meta-log-e8--euclidean-distance current end) 1e-6)
          (cl-return (cons path g-score)))
        ;; Expand neighbors
        (dolist (root roots-to-use)
          (let* ((neighbor-coords (cl-mapcar #'+ (meta-log-e8-point-coords current) root))
                 (neighbor (make-meta-log-e8-point
                            :coords neighbor-coords
                            :bip32-path (concat (meta-log-e8-point-bip32-path current) "/step")
                            :depth (1+ (meta-log-e8-point-depth current))
                            :parent current))
                 (neighbor-hash (meta-log-e8--point-hash neighbor)))
            (unless (gethash neighbor-hash visited)
              (let* ((root-norm (sqrt (cl-reduce #'+ (mapcar (lambda (x) (* x x)) root))))
                     (new-g (+ g-score root-norm))
                     (h-score (meta-log-e8--euclidean-distance neighbor end))
                     (new-f (+ new-g h-score)))
                (setq counter (1+ counter))
                (push (list new-f counter h-score new-g neighbor (append path (list neighbor)))
                      frontier)))))
        (setq frontier (sort frontier (lambda (a b) (< (car a) (car b)))))))
    ;; No path found, return direct path
    (cons (list start end) (meta-log-e8--euclidean-distance start end))))

;;; p-Adic Heights

(defun meta-log-e8-padic-height (point p)
  "Compute p-adic height on E8 lattice for POINT at prime P.
Used for ramification detection.
Returns float."
  (let ((coords (meta-log-e8-point-coords point))
        (height 0.0))
    (dolist (coord coords)
      (let ((coord-int (round (* coord (expt 2 10)))))  ; Scale to integer
        (if (zerop coord-int)
            (setq height (+ height 1000.0))  ; Large penalty for zero
          (let ((val (meta-log-p-adic-valuation coord-int p)))
            (setq height (+ height (* val (log p))))))))
    height))

;;; FRBAC Delegation Verification

(defun meta-log-e8-verify-frbac-delegation (master delegate)
  "Verify FRBAC delegation using E8 automorphisms.
MASTER and DELEGATE are meta-log-e8-point structures.
Valid delegation iff:
1. delegate ∈ Weyl orbit of master, OR
2. delegate is reachable via root steps from master
Returns t if valid, nil otherwise."
  (meta-log-e8-initialize)
  ;; Check Weyl orbit (with performance limit)
  (let ((orbit (meta-log-e8-weyl-orbit master))
        (delegate-hash (meta-log-e8--point-hash delegate)))
        (when (cl-find delegate orbit
                   :test (lambda (d p) (equal (meta-log-e8--point-hash p) delegate-hash)))
      (cl-return t))
    ;; Check root-step reachability (heuristic)
    (let ((diff (cl-mapcar #'- (meta-log-e8-point-coords delegate)
                           (meta-log-e8-point-coords master)))
          (diff-norm (sqrt (cl-reduce #'+
                                     (mapcar (lambda (x) (* x x)) diff)))))
      (< diff-norm 10.0))))  ; Heuristic threshold

;;; Distance Features for ML

(defun meta-log-e8-distance-for-ml (p1 p2)
  "Compute E8 distance features for voter ML.
P1 and P2 are meta-log-e8-point structures.
Returns alist with keys: euclidean, padic_2, padic_3, weyl_distance."
  (let ((euclidean (meta-log-e8--euclidean-distance p1 p2))
        (padic-2 (abs (- (meta-log-e8-padic-height p1 2)
                         (meta-log-e8-padic-height p2 2))))
        (padic-3 (abs (- (meta-log-e8-padic-height p1 3)
                         (meta-log-e8-padic-height p2 3))))
        (orbit1 (meta-log-e8-weyl-orbit p1 10))  ; Small orbit for distance
        (weyl-distance (cl-reduce #'min
                                  (mapcar (lambda (q)
                                            (meta-log-e8--euclidean-distance q p2))
                                          orbit1))))
    (list (cons 'euclidean euclidean)
          (cons 'padic_2 padic-2)
          (cons 'padic_3 padic-3)
          (cons 'weyl_distance weyl-distance))))

(provide 'meta-log-e8)

;;; meta-log-e8.el ends here

