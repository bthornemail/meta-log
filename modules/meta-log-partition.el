;;; meta-log-partition.el --- Network partition detection via Betti numbers

;; Copyright (C) 2025 Automaton System
;; Author: Automaton System
;; Version: 1.0.0

;; This file is part of meta-log.

;; meta-log is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; Network partition detection using topological invariants (Betti numbers).
;; Implements O(v) partition detection via β₀ (connected components).
;; Provides geometric decomposition and dual-based partition recovery.

;;; Code:

(require 'cl-lib)

(defvar meta-log-partition--decomposition-map
  '((24-cell . (cuboctahedron . 2))
    (cuboctahedron . (triangle . 2))
    (octahedron . (triangle . 2))
    (cube . (tetrahedron . 2))
    (tetrahedron . (line . 2)))
  "Geometric decomposition mapping for partition handling.")

(defun meta-log-partition-calculate-betti-0 (vertices edges)
  "Calculate Betti number β₀ (connected components).
VERTICES is a list of vertex identifiers.
EDGES is a list of (from . to) pairs.
Returns β₀ (number of connected components)."
  (let ((visited (make-hash-table :test 'equal))
        (components 0))
    ;; Build adjacency list
    (let ((adj (make-hash-table :test 'equal)))
      (dolist (v vertices)
        (puthash v '() adj))
      (dolist (edge edges)
        (let ((from (car edge))
              (to (cdr edge)))
          (push to (gethash from adj))
          (push from (gethash to adj))))
      ;; DFS to find connected components
      (dolist (v vertices)
        (unless (gethash v visited)
          (meta-log-partition--dfs v adj visited)
          (setq components (1+ components)))))
    components))

(defun meta-log-partition--dfs (vertex adj visited)
  "Depth-first search helper for connected components."
  (puthash vertex t visited)
  (dolist (neighbor (gethash vertex adj))
    (unless (gethash neighbor visited)
      (meta-log-partition--dfs neighbor adj visited))))

(defun meta-log-partition-detect (vertices edges)
  "Detect network partition via Betti number β₀.
VERTICES is a list of vertex identifiers.
EDGES is a list of (from . to) pairs.
Returns partition info alist with keys:
  :is-partitioned - boolean
  :partition-count - β₀ value
  :components - list of component vertex sets"
  (let ((beta-0 (meta-log-partition-calculate-betti-0 vertices edges))
        (components (meta-log-partition--extract-components vertices edges)))
    `((:is-partitioned . ,(> beta-0 1))
      (:partition-count . ,beta-0)
      (:components . ,components)
      (:betti-0 . ,beta-0))))

(defun meta-log-partition--extract-components (vertices edges)
  "Extract connected components from graph.
Returns list of component vertex lists."
  (let ((visited (make-hash-table :test 'equal))
        (components '())
        (adj (make-hash-table :test 'equal)))
    ;; Build adjacency list
    (dolist (v vertices)
      (puthash v '() adj))
    (dolist (edge edges)
      (let ((from (car edge))
            (to (cdr edge)))
        (push to (gethash from adj))
        (push from (gethash to adj))))
    ;; Extract components
    (dolist (v vertices)
      (unless (gethash v visited)
        (push (meta-log-partition--dfs-collect v adj visited) components))))
    components))

(defun meta-log-partition--dfs-collect (vertex adj visited)
  "DFS helper that collects vertices into component.
Returns list of vertices in component."
  (puthash vertex t visited)
  (let ((component (list vertex)))
    (dolist (neighbor (gethash vertex adj))
      (unless (gethash neighbor visited)
        (setq component (append component (meta-log-partition--dfs-collect neighbor adj visited)))))
    component))

(defun meta-log-partition-decompose (geometric-type partition-count)
  "Decompose geometric type under partition.
GEOMETRIC-TYPE is a symbol (cube, octahedron, etc.).
PARTITION-COUNT is β₀ (number of partitions).
Returns decomposed type symbol."
  (let ((entry (assq geometric-type meta-log-partition--decomposition-map)))
    (if entry
        (let ((decomp (cdr entry)))
          (if (eq partition-count (cdr decomp))
              (car decomp)
            geometric-type))
      geometric-type)))

(defun meta-log-partition-recover (partition-certs original-type)
  "Recover unified consensus from partition states using duality.
PARTITION-CERTS is a list of consensus certificates from each partition.
ORIGINAL-TYPE is the original geometric type (pre-partition).
Returns unified consensus certificate alist."
  (let ((total-agrees 0)
        (total-vertices 0))
    (dolist (cert partition-certs)
      (setq total-agrees (+ total-agrees (or (cdr (assq :agrees-count cert)) 0)))
      (setq total-vertices (+ total-vertices (or (cdr (assq :vertices cert)) 0))))
    `((:geometric-type . ,original-type)
      (:agrees-count . ,total-agrees)
      (:total-vertices . ,total-vertices)
      (:valid . ,(>= total-agrees (floor (* total-vertices 0.5))))
      (:recovery-method . geometric-duality))))

(provide 'meta-log-partition)

