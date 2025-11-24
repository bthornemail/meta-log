;;; common/helpers.scm --- Shared Helper Functions
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Shared utility functions used across multiple Scheme modules.
;;; This module provides common helpers to reduce code duplication.

;;; Code:

;; Helper function to call Emacs Lisp from Scheme
;; This is used by multiple modules (prolog-interface, vision, etc.)
(define (call-emacs-lisp function-name args)
  "Call Emacs Lisp function from Scheme.
FUNCTION-NAME: string naming the Emacs Lisp function
ARGS: list of arguments
Returns a marker that Emacs side intercepts to execute the function."
  (list 'emacs-lisp-call function-name args))

;; Helper: Extract value from alist by key
(define (assoc-ref alist key)
  "Get value from alist by key.
ALIST: association list
KEY: key to look up
Returns value or #f if not found."
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
        #f)))

;; Helper: Find pair with key in alist
(define (assoc key alist)
  "Find pair with key in alist.
KEY: key to search for
ALIST: association list
Returns pair or #f if not found."
  (if (null? alist)
      #f
      (if (equal? (caar alist) key)
          (car alist)
          (assoc key (cdr alist)))))

;; Helper: Remove item from list
(define (remove item list)
  "Remove first occurrence of item from list.
ITEM: item to remove
LIST: list to remove from
Returns new list without item."
  (if (null? list)
      '()
      (if (equal? (car list) item)
          (cdr list)
          (cons (car list) (remove item (cdr list))))))

;; Helper: Filter list by predicate
(define (filter pred lst)
  "Filter list by predicate.
PRED: predicate function
LST: list to filter
Returns filtered list."
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter pred (cdr lst)))
          (filter pred (cdr lst)))))

;; Helper: Take first n elements from list
(define (take list n)
  "Take first n elements from list.
LIST: list to take from
N: number of elements
Returns list of first n elements."
  (if (or (null? list) (<= n 0))
      '()
      (cons (car list) (take (cdr list) (- n 1)))))

;; Helper: Create list of n copies of value
(define (make-list n value)
  "Create list of n copies of value.
N: number of elements
VALUE: value to repeat
Returns list."
  (if (<= n 0)
      '()
      (cons value (make-list (- n 1) value))))

;; Helper: Generate list of integers from 0 to n-1
(define (iota n)
  "Generate list of integers from 0 to n-1.
N: number of integers
Returns list (0 1 2 ... n-1)."
  (let loop ((i 0)
             (result '()))
    (if (>= i n)
        (reverse result)
        (loop (+ i 1) (cons i result)))))

;; Helper: Take every nth element starting at offset
(define (take-every-nth list n offset)
  "Take every nth element from list starting at offset.
LIST: list to take from
N: step size
OFFSET: starting offset
Returns list of selected elements."
  (if (null? list)
      '()
      (if (zero? offset)
          (cons (car list) (take-every-nth (cdr list) n (- n 1)))
          (take-every-nth (cdr list) n (- offset 1)))))

;; Helper: Convert real number to complex (real, 0)
(define (real-to-complex x)
  "Convert real number to complex representation.
X: real number (or already complex list)
Returns list (real imag) representing complex number."
  (if (list? x)
      x  ; Already complex
      (list x 0.0)))

;; Functions are exported by default in R5RS

