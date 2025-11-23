;;; meta-log-quadratic-forms.el --- Binary, Ternary, and Quaternary Quadratic Forms

;; Copyright (C) 2025 Automaton System
;; Author: Automaton System
;; Version: 1.0.0

;; This file is part of meta-log.

;; meta-log is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; Implementation of quadratic forms (BQF, TQF, QQF) with discriminant calculations.
;; Integrates with CanvasL automaton definitions and geometric consensus.

;;; Code:

(require 'cl-lib)

;;; Binary Quadratic Forms (BQF)

(cl-defstruct meta-log-bqf
  "Binary quadratic form structure: ax² + bxy + cy²"
  a b c)

(defun meta-log-bqf-create (a b c)
  "Create a binary quadratic form: ax² + bxy + cy².
A, B, C are coefficients.
Returns meta-log-bqf structure."
  (make-meta-log-bqf :a a :b b :c c))

(defun meta-log-bqf-discriminant (bqf)
  "Calculate discriminant Δ = b² - 4ac for BQF.
BQF is a meta-log-bqf structure.
Returns discriminant value."
  (let ((a (meta-log-bqf-a bqf))
        (b (meta-log-bqf-b bqf))
        (c (meta-log-bqf-c bqf)))
    (- (* b b) (* 4 a c))))

(defun meta-log-bqf-classify (bqf)
  "Classify BQF by discriminant.
BQF is a meta-log-bqf structure.
Returns symbol: 'positive-definite, 'negative-definite, 'indefinite, or 'degenerate."
  (let ((delta (meta-log-bqf-discriminant bqf))
        (a (meta-log-bqf-a bqf)))
    (cond
     ((= delta 0) 'degenerate)
     ((< delta 0)
      (if (> a 0) 'positive-definite 'negative-definite))
     (t 'indefinite))))

(defun meta-log-bqf-from-coefficients (coefficients)
  "Create BQF from coefficient list [a, b, c].
COEFFICIENTS is a list of 3 numbers.
Returns meta-log-bqf structure."
  (when (and (listp coefficients) (= (length coefficients) 3))
    (meta-log-bqf-create (nth 0 coefficients)
                        (nth 1 coefficients)
                        (nth 2 coefficients))))

(defun meta-log-bqf-from-canvasl (bqf-obj)
  "Extract BQF from CanvasL bipartite.bqf object.
BQF-OBJ is a parsed JSON object with 'coefficients and 'form fields.
Returns meta-log-bqf structure or nil."
  (when bqf-obj
    (let ((coeffs (cdr (assq 'coefficients bqf-obj))))
      (cond
       ((vectorp coeffs)
        ;; Convert vector to list
        (let ((coeff-list (cl-loop for i from 0 below (length coeffs)
                                   collect (aref coeffs i))))
          (when (>= (length coeff-list) 3)
            (meta-log-bqf-from-coefficients coeff-list))))
       ((listp coeffs)
        (when (>= (length coeffs) 3)
          (meta-log-bqf-from-coefficients coeffs)))))))

;;; Ternary Quadratic Forms (TQF)

(cl-defstruct meta-log-tqf
  "Ternary quadratic form structure: ax² + by² + cz² + 2dxy + 2exz + 2fyz"
  a b c d e f)

(defun meta-log-tqf-create (a b c d e f)
  "Create a ternary quadratic form.
A, B, C are diagonal coefficients.
D, E, F are cross-term coefficients (halved).
Returns meta-log-tqf structure."
  (make-meta-log-tqf :a a :b b :c c :d d :e e :f f))

(defun meta-log-tqf-discriminant (tqf)
  "Calculate discriminant Δ for TQF.
TQF is a meta-log-tqf structure.
Formula: Δ = a*b*c - a*f² - b*e² - c*d² + 2*d*e*f
Returns discriminant value."
  (let ((a (meta-log-tqf-a tqf))
        (b (meta-log-tqf-b tqf))
        (c (meta-log-tqf-c tqf))
        (d (meta-log-tqf-d tqf))
        (e (meta-log-tqf-e tqf))
        (f (meta-log-tqf-f tqf)))
    (- (+ (* a b c) (* 2 d e f))
       (+ (* a f f) (* b e e) (* c d d)))))

(defun meta-log-tqf-classify (tqf)
  "Classify TQF by discriminant.
TQF is a meta-log-tqf structure.
Returns symbol: 'positive-definite, 'indefinite, or 'degenerate."
  (let ((delta (meta-log-tqf-discriminant tqf)))
    (cond
     ((= delta 0) 'degenerate)
     ((> delta 0) 'positive-definite)
     (t 'indefinite))))

;;; Quaternary Quadratic Forms (QQF)

(cl-defstruct meta-log-qqf
  "Quaternary quadratic form structure.
Matrix form: q(X) = Xᵀ A X where A is 4x4 symmetric matrix."
  matrix)

(defun meta-log-qqf-create (a b c d e f g h i j)
  "Create a quaternary quadratic form.
Coefficients: a, b, c, d (diagonal), e, f, g, h, i, j (cross terms).
Matrix form: [[a, e, f, g], [e, b, h, i], [f, h, c, j], [g, i, j, d]]
Returns meta-log-qqf structure."
  (make-meta-log-qqf
   :matrix (vector (vector a e f g)
                   (vector e b h i)
                   (vector f h c j)
                   (vector g i j d))))

(defun meta-log-qqf-discriminant (qqf)
  "Calculate discriminant Δ = det(A) for QQF.
QQF is a meta-log-qqf structure.
Returns determinant of the 4x4 matrix."
  (let ((m (meta-log-qqf-matrix qqf)))
    ;; Compute 4x4 determinant
    (meta-log-matrix-determinant-4x4 m)))

(defun meta-log-matrix-determinant-4x4 (m)
  "Compute determinant of 4x4 matrix M.
M is a vector of 4 vectors.
Returns determinant value."
  (let ((a00 (aref (aref m 0) 0))
        (a01 (aref (aref m 0) 1))
        (a02 (aref (aref m 0) 2))
        (a03 (aref (aref m 0) 3))
        (a10 (aref (aref m 1) 0))
        (a11 (aref (aref m 1) 1))
        (a12 (aref (aref m 1) 2))
        (a13 (aref (aref m 1) 3))
        (a20 (aref (aref m 2) 0))
        (a21 (aref (aref m 2) 1))
        (a22 (aref (aref m 2) 2))
        (a23 (aref (aref m 2) 3))
        (a30 (aref (aref m 3) 0))
        (a31 (aref (aref m 3) 1))
        (a32 (aref (aref m 3) 2))
        (a33 (aref (aref m 3) 3)))
    ;; Laplace expansion: det = Σ (-1)^(i+j) * a_ij * M_ij
    (+ (* a00 (meta-log-matrix-determinant-3x3
               (vector (vector a11 a12 a13)
                       (vector a21 a22 a23)
                       (vector a31 a32 a33))))
       (- (* a01 (meta-log-matrix-determinant-3x3
                  (vector (vector a10 a12 a13)
                          (vector a20 a22 a23)
                          (vector a30 a32 a33)))))
       (* a02 (meta-log-matrix-determinant-3x3
               (vector (vector a10 a11 a13)
                       (vector a20 a21 a23)
                       (vector a30 a31 a33))))
       (- (* a03 (meta-log-matrix-determinant-3x3
                  (vector (vector a10 a11 a12)
                          (vector a20 a21 a22)
                          (vector a30 a31 a32))))))))

(defun meta-log-matrix-determinant-3x3 (m)
  "Compute determinant of 3x3 matrix M.
Formula: a(ei - fh) - b(di - fg) + c(dh - eg)"
  (let ((a (aref (aref m 0) 0))
        (b (aref (aref m 0) 1))
        (c (aref (aref m 0) 2))
        (d (aref (aref m 1) 0))
        (e (aref (aref m 1) 1))
        (f (aref (aref m 1) 2))
        (g (aref (aref m 2) 0))
        (h (aref (aref m 2) 1))
        (i (aref (aref m 2) 2)))
    (- (+ (* a (- (* e i) (* f h)))
          (* b (- (* d i) (* f g))))
       (* c (- (* d h) (* e g))))))

(defun meta-log-qqf-classify (qqf)
  "Classify QQF by discriminant.
QQF is a meta-log-qqf structure.
Returns symbol: 'positive-definite, 'indefinite, or 'degenerate."
  (let ((delta (meta-log-qqf-discriminant qqf)))
    (cond
     ((= delta 0) 'degenerate)
     ((> delta 0) 'positive-definite)
     (t 'indefinite))))

;;; Integration with Geometric Consensus

(defun meta-log-quadratic-forms-classify-consensus (form-type form-data)
  "Classify consensus stability using quadratic form discriminant.
FORM-TYPE is 'bqf, 'tqf, or 'qqf.
FORM-DATA is the form structure or CanvasL data.
Returns classification result with stability prediction."
  (let ((classification
         (pcase form-type
           ('bqf
            (let ((bqf (if (meta-log-bqf-p form-data)
                          form-data
                        (meta-log-bqf-from-canvasl form-data))))
              (when bqf
                (meta-log-bqf-classify bqf))))
           ('tqf
            (when (meta-log-tqf-p form-data)
              (meta-log-tqf-classify form-data)))
           ('qqf
            (when (meta-log-qqf-p form-data)
              (meta-log-qqf-classify form-data))))))
    (when classification
      `((:classification . ,classification)
        (:stable . ,(member classification '(positive-definite negative-definite)))
        (:indefinite . ,(eq classification 'indefinite))
        (:degenerate . ,(eq classification 'degenerate))))))

(defun meta-log-qqf-e8-theta-link (qqf &optional theta-series)
  "Link QQF to E8 theta series for enhanced analysis.
QQF is a meta-log-qqf structure.
THETA-SERIES is optional E8 theta series (default: create with max-norm=10).
Returns plist with E8 theta analysis: :determinant, :predicted-universality,
:theta-growth-rate, :ramanujan-type."
  (require 'meta-log-e8-theta)
  (let* ((theta (or theta-series (meta-log-e8-theta-series-create 10)))
         (matrix (meta-log-qqf-matrix qqf))
         (matrix-list (cl-loop for i from 0 below 4
                              collect (cl-loop for j from 0 below 4
                                             collect (float (aref (aref matrix i) j))))))
    (meta-log-e8-theta-link-to-qqf theta matrix-list)))

(provide 'meta-log-quadratic-forms)

;;; meta-log-quadratic-forms.el ends here

