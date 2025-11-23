;;; meta-log-benchmark.el --- Benchmarking utilities for meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Automaton System
;; Version: 1.0.0

;; This file is part of meta-log.

;; meta-log is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; Benchmarking utilities for meta-log operations.
;; Provides timing, memory usage, and performance metrics.

;;; Code:

(require 'cl-lib)

;;; Benchmark Structure

(cl-defstruct meta-log-benchmark-result
  "Benchmark result structure."
  name
  start-time
  end-time
  elapsed-time
  memory-before
  memory-after
  memory-delta
  iterations
  operations-per-second
  metadata)

;;; Benchmark State

(defvar meta-log-benchmark--results (make-hash-table :test 'equal)
  "Storage for benchmark results.")

(defvar meta-log-benchmark--active nil
  "Currently active benchmark name, or nil.")

;;; Memory Measurement

(defun meta-log-benchmark-get-memory ()
  "Get current memory usage in bytes.
Returns approximate memory usage."
  (if (fboundp 'garbage-collect)
      (progn
        (garbage-collect)
        (let ((stats (garbage-collect)))
          (if (and (consp stats) (numberp (car stats)))
              (* (car stats) 1024)  ; Convert KB to bytes
            0)))
    0))

;;; Benchmarking Functions

(defun meta-log-benchmark-start (name &optional metadata)
  "Start a benchmark.
NAME is the benchmark name.
METADATA is optional plist with additional information.
Returns benchmark result structure."
  (when meta-log-benchmark--active
    (message "Warning: Benchmark '%s' already active, starting '%s'"
             meta-log-benchmark--active name))
  
  (let* ((start-time (current-time))
         (memory-before (meta-log-benchmark-get-memory))
         (result (make-meta-log-benchmark-result
                  :name name
                  :start-time start-time
                  :memory-before memory-before
                  :metadata (or metadata '()))))
    (setq meta-log-benchmark--active name)
    (puthash name result meta-log-benchmark--results)
    result))

(defun meta-log-benchmark-end (name &optional iterations)
  "End a benchmark.
NAME is the benchmark name.
ITERATIONS is optional number of iterations (for ops/sec calculation).
Returns benchmark result structure."
  (let ((result (gethash name meta-log-benchmark--results)))
    (unless result
      (error "Benchmark '%s' not found" name))
    
    (let* ((end-time (current-time))
           (memory-after (meta-log-benchmark-get-memory))
           (elapsed-time (float-time (time-subtract end-time
                                                    (meta-log-benchmark-result-start-time result))))
           (memory-delta (- memory-after (meta-log-benchmark-result-memory-before result)))
           (ops-per-sec (if (and iterations (> iterations 0) (> elapsed-time 0))
                            (/ iterations elapsed-time)
                          0)))
      
      (setf (meta-log-benchmark-result-end-time result) end-time)
      (setf (meta-log-benchmark-result-elapsed-time result) elapsed-time)
      (setf (meta-log-benchmark-result-memory-after result) memory-after)
      (setf (meta-log-benchmark-result-memory-delta result) memory-delta)
      (setf (meta-log-benchmark-result-iterations result) (or iterations 0))
      (setf (meta-log-benchmark-result-operations-per-second result) ops-per-sec)
      
      (when (eq meta-log-benchmark--active name)
        (setq meta-log-benchmark--active nil))
      
      result)))

(defun meta-log-benchmark-run (name func &optional iterations metadata)
  "Run a benchmark.
NAME is the benchmark name.
FUNC is a function to benchmark (called ITERATIONS times).
ITERATIONS is number of iterations (default 1).
METADATA is optional plist with additional information.
Returns benchmark result structure."
  (let ((iterations (or iterations 1))
        (result (meta-log-benchmark-start name metadata)))
    (dotimes (i iterations)
      (funcall func))
    (meta-log-benchmark-end name iterations)
    result))

;;; Benchmark Reporting

(defun meta-log-benchmark-report (name)
  "Report benchmark results.
NAME is the benchmark name.
Prints formatted report."
  (let ((result (gethash name meta-log-benchmark--results)))
    (unless result
      (error "Benchmark '%s' not found" name))
    
    (let ((elapsed (meta-log-benchmark-result-elapsed-time result))
          (memory-delta (meta-log-benchmark-result-memory-delta result))
          (ops-per-sec (meta-log-benchmark-result-operations-per-second result))
          (iterations (meta-log-benchmark-result-iterations result)))
      
      (message "")
      (message "╔════════════════════════════════════════════════════════════╗")
      (message "║         Benchmark: %-40s ║" name)
      (message "╚════════════════════════════════════════════════════════════╝")
      (message "")
      (message "Elapsed Time:    %.6f seconds" elapsed)
      (when (> iterations 0)
        (message "Iterations:      %d" iterations)
        (message "Ops/Second:      %.2f" ops-per-sec))
      (message "Memory Delta:    %d bytes (%.2f KB)" 
               memory-delta 
               (/ memory-delta 1024.0))
      (when (meta-log-benchmark-result-metadata result)
        (message "Metadata:        %s" (meta-log-benchmark-result-metadata result)))
      (message ""))))

(defun meta-log-benchmark-report-all ()
  "Report all benchmark results."
  (interactive)
  (message "")
  (message "╔════════════════════════════════════════════════════════════╗")
  (message "║         All Benchmark Results                              ║")
  (message "╚════════════════════════════════════════════════════════════╝")
  (message "")
  
  (maphash (lambda (name result)
             (meta-log-benchmark-report name))
           meta-log-benchmark--results)
  
  (message "Total Benchmarks: %d" (hash-table-count meta-log-benchmark--results)))

(defun meta-log-benchmark-clear ()
  "Clear all benchmark results."
  (interactive)
  (clrhash meta-log-benchmark--results)
  (setq meta-log-benchmark--active nil)
  (message "Benchmark results cleared"))

;;; Convenience Macros

(defmacro meta-log-benchmark-time (name &rest body)
  "Benchmark execution of BODY.
NAME is the benchmark name.
Returns the result of BODY."
  `(let ((result (meta-log-benchmark-start ,name)))
     (unwind-protect
         (progn ,@body)
       (meta-log-benchmark-end ,name)
       (meta-log-benchmark-report ,name))))

(provide 'meta-log-benchmark)
;;; meta-log-benchmark.el ends here

