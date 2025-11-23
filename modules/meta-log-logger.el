;;; meta-log-logger.el --- Logging utilities for meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Automaton System
;; Version: 1.0.0

;; This file is part of meta-log.

;; meta-log is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; Logging utilities for meta-log operations.
;; Provides structured logging with levels, categories, and file output.

;;; Code:

(require 'cl-lib)

;;; Log Levels

(defconst meta-log-log-level-debug 0
  "Debug log level.")

(defconst meta-log-log-level-info 1
  "Info log level.")

(defconst meta-log-log-level-warn 2
  "Warning log level.")

(defconst meta-log-log-level-error 3
  "Error log level.")

(defconst meta-log-log-level-fatal 4
  "Fatal log level.")

;;; Log Entry Structure

(cl-defstruct meta-log-log-entry
  "Log entry structure."
  timestamp
  level
  category
  message
  metadata)

;;; Logger Configuration

(defvar meta-log-logger--level meta-log-log-level-info
  "Current log level threshold.")

(defvar meta-log-logger--categories (make-hash-table :test 'equal)
  "Enabled log categories (hash table of category -> t/nil).")

(defvar meta-log-logger--entries '()
  "In-memory log entries (circular buffer).")

(defvar meta-log-logger--max-entries 1000
  "Maximum number of log entries to keep in memory.")

(defvar meta-log-logger--file nil
  "Log file path, or nil to disable file logging.")

(defvar meta-log-logger--file-buffer nil
  "Buffer for log file output.")

;;; Log Level Names

(defun meta-log-logger-level-name (level)
  "Get name for log LEVEL.
Returns string like 'DEBUG', 'INFO', etc."
  (pcase level
    (0 "DEBUG")
    (1 "INFO")
    (2 "WARN")
    (3 "ERROR")
    (4 "FATAL")
    (_ "UNKNOWN")))

;;; Logging Functions

(defun meta-log-logger-log (level category message &optional metadata)
  "Log a message.
LEVEL is the log level (0-4).
CATEGORY is the log category (string).
MESSAGE is the log message (string).
METADATA is optional plist with additional information."
  (when (>= level meta-log-logger--level)
    (let ((category-enabled (gethash category meta-log-logger--categories t)))
      (when (or (null category-enabled) category-enabled)
          (let* ((timestamp (current-time))
                 (entry (make-meta-log-log-entry
                         :timestamp timestamp
                         :level level
                         :category category
                         :message message
                         :metadata (or metadata '())))
                 (level-name (meta-log-logger-level-name level)))
            
            ;; Add to in-memory buffer
            (push entry meta-log-logger--entries)
            (when (> (length meta-log-logger--entries) meta-log-logger--max-entries)
              (setq meta-log-logger--entries
                    (butlast meta-log-logger--entries
                             (- (length meta-log-logger--entries)
                                meta-log-logger--max-entries))))
            
            ;; Write to file if enabled
            (when meta-log-logger--file
              (meta-log-logger-write-entry entry))
            
            ;; Print to message buffer
            (let ((formatted (format "[%s] [%s] %s: %s"
                                   (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)
                                   level-name
                                   category
                                   message)))
              (message "%s" formatted)))))))

(defun meta-log-logger-write-entry (entry)
  "Write log ENTRY to file.
ENTRY is a meta-log-log-entry structure."
  (when meta-log-logger--file
    (let ((file-exists (file-exists-p meta-log-logger--file))
          (level-name (meta-log-logger-level-name (meta-log-log-entry-level entry)))
          (timestamp (meta-log-log-entry-timestamp entry))
          (category (meta-log-log-entry-category entry))
          (message (meta-log-log-entry-message entry))
          (metadata (meta-log-log-entry-metadata entry)))
      
      (with-temp-buffer
        (insert (format "[%s] [%s] [%s] %s"
                        (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)
                        level-name
                        category
                        message))
        (when metadata
          (insert (format " | %s" metadata)))
        (insert "\n")
        
        (write-region (point-min) (point-max)
                      meta-log-logger--file
                      (if file-exists 'append nil)
                      'no-message)))))

;;; Convenience Functions

(defun meta-log-logger-debug (category message &optional metadata)
  "Log a debug message.
CATEGORY is the log category.
MESSAGE is the log message.
METADATA is optional plist."
  (meta-log-logger-log meta-log-log-level-debug category message metadata))

(defun meta-log-logger-info (category message &optional metadata)
  "Log an info message.
CATEGORY is the log category.
MESSAGE is the log message.
METADATA is optional plist."
  (meta-log-logger-log meta-log-log-level-info category message metadata))

(defun meta-log-logger-warn (category message &optional metadata)
  "Log a warning message.
CATEGORY is the log category.
MESSAGE is the log message.
METADATA is optional plist."
  (meta-log-logger-log meta-log-log-level-warn category message metadata))

(defun meta-log-logger-error (category message &optional metadata)
  "Log an error message.
CATEGORY is the log category.
MESSAGE is the log message.
METADATA is optional plist."
  (meta-log-logger-log meta-log-log-level-error category message metadata))

(defun meta-log-logger-fatal (category message &optional metadata)
  "Log a fatal message.
CATEGORY is the log category.
MESSAGE is the log message.
METADATA is optional plist."
  (meta-log-logger-log meta-log-log-level-fatal category message metadata))

;;; Configuration

(defun meta-log-logger-set-level (level)
  "Set log level threshold.
LEVEL is 0 (DEBUG) to 4 (FATAL)."
  (setq meta-log-logger--level level)
  (message "Log level set to %s" (meta-log-logger-level-name level)))

(defun meta-log-logger-set-file (file-path)
  "Set log file path.
FILE-PATH is the path to the log file, or nil to disable file logging."
  (setq meta-log-logger--file file-path)
  (if file-path
      (message "Log file set to: %s" file-path)
    (message "File logging disabled")))

(defun meta-log-logger-enable-category (category)
  "Enable logging for CATEGORY.
CATEGORY is a string."
  (puthash category t meta-log-logger--categories)
  (message "Logging enabled for category: %s" category))

(defun meta-log-logger-disable-category (category)
  "Disable logging for CATEGORY.
CATEGORY is a string."
  (puthash category nil meta-log-logger--categories)
  (message "Logging disabled for category: %s" category))

;;; Log Querying

(defun meta-log-logger-get-entries (&optional level category limit)
  "Get log entries.
LEVEL is optional log level filter.
CATEGORY is optional category filter.
LIMIT is optional maximum number of entries.
Returns list of log entries."
  (let ((filtered '())
        (count 0))
    (dolist (entry (reverse meta-log-logger--entries))
      (when (and (or (null level) (= (meta-log-log-entry-level entry) level))
                 (or (null category) (string= (meta-log-log-entry-category entry) category))
                 (or (null limit) (< count limit)))
        (push entry filtered)
        (setq count (1+ count))))
    filtered))

(defun meta-log-logger-print-entries (&optional level category limit)
  "Print log entries.
LEVEL is optional log level filter.
CATEGORY is optional category filter.
LIMIT is optional maximum number of entries."
  (let ((entries (meta-log-logger-get-entries level category limit)))
    (message "")
    (message "╔════════════════════════════════════════════════════════════╗")
    (message "║         Log Entries (%d)                                  ║" (length entries))
    (message "╚════════════════════════════════════════════════════════════╝")
    (message "")
    (dolist (entry entries)
      (let ((level-name (meta-log-logger-level-name (meta-log-log-entry-level entry)))
            (timestamp (meta-log-log-entry-timestamp entry))
            (category (meta-log-log-entry-category entry))
            (message (meta-log-log-entry-message entry)))
        (message "[%s] [%s] [%s] %s"
                 (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)
                 level-name
                 category
                 message)))
    (message "")))

(defun meta-log-logger-clear ()
  "Clear in-memory log entries."
  (interactive)
  (setq meta-log-logger--entries '())
  (message "Log entries cleared"))

(provide 'meta-log-logger)
;;; meta-log-logger.el ends here

