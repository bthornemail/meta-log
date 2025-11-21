;;; meta-log-llm-cache.el --- Query translation cache for meta-log LLM

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Caching system for LLM query translations:
;; - Instant lookup for repeated queries
;; - Tracks usage patterns
;; - Learns from successful translations
;; - Persistent storage across sessions

;;; Code:

(require 'cl-lib)

;;; Configuration

(defcustom meta-log-llm-cache-size 1000
  "Maximum number of cached query translations."
  :type 'integer
  :group 'meta-log-llm)

(defcustom meta-log-llm-cache-file
  (expand-file-name "meta-log-llm-cache.el" user-emacs-directory)
  "File to persist query cache."
  :type 'file
  :group 'meta-log-llm)

;;; Cache Storage

(defvar meta-log-llm--cache (make-hash-table :test 'equal)
  "Hash table storing cached query translations.
Key: normalized query string
Value: meta-log-llm-translation-cache struct")

(defvar meta-log-llm--cache-stats
  (list :hits 0
        :misses 0
        :local-count 0
        :remote-count 0)
  "Cache statistics.")

;;; Cache Operations

(defun meta-log-llm-cache-lookup (query)
  "Look up QUERY in cache.
Returns meta-log-llm-query-result if found, nil otherwise."
  (let* ((normalized (meta-log-llm-cache--normalize-query query))
         (entry (gethash normalized meta-log-llm--cache)))

    (if entry
        (progn
          ;; Cache hit
          (plist-put meta-log-llm--cache-stats :hits
                    (1+ (plist-get meta-log-llm--cache-stats :hits)))

          ;; Update usage statistics
          (setf (meta-log-llm-translation-cache-usage-count entry)
                (1+ (meta-log-llm-translation-cache-usage-count entry)))
          (setf (meta-log-llm-translation-cache-last-used entry)
                (current-time))

          ;; Return result with updated source
          (let ((result (meta-log-llm-translation-cache-result entry)))
            (setf (meta-log-llm-query-result-source result) 'cache)
            (setf (meta-log-llm-query-result-timestamp result) (current-time))
            result))

      ;; Cache miss
      (plist-put meta-log-llm--cache-stats :misses
                (1+ (plist-get meta-log-llm--cache-stats :misses)))
      nil)))

(defun meta-log-llm-cache-store (query result)
  "Store QUERY translation RESULT in cache."
  (let* ((normalized (meta-log-llm-cache--normalize-query query))
         (existing (gethash normalized meta-log-llm--cache)))

    (if existing
        ;; Update existing entry
        (progn
          (setf (meta-log-llm-translation-cache-result existing) result)
          (setf (meta-log-llm-translation-cache-usage-count existing)
                (1+ (meta-log-llm-translation-cache-usage-count existing)))
          (setf (meta-log-llm-translation-cache-last-used existing)
                (current-time)))

      ;; Create new entry
      (let ((entry (make-meta-log-llm-translation-cache
                    :query normalized
                    :result result
                    :usage-count 1
                    :last-used (current-time)
                    :success-rate 1.0)))

        (puthash normalized entry meta-log-llm--cache)

        ;; Enforce cache size limit
        (when (> (hash-table-count meta-log-llm--cache) meta-log-llm-cache-size)
          (meta-log-llm-cache-evict-lru))))))

(defun meta-log-llm-cache--normalize-query (query)
  "Normalize QUERY for cache lookup.
Lowercases, removes extra whitespace, etc."
  (let ((normalized query))
    ;; Convert to lowercase
    (setq normalized (downcase normalized))
    ;; Remove leading/trailing whitespace
    (setq normalized (string-trim normalized))
    ;; Collapse multiple spaces
    (setq normalized (replace-regexp-in-string "  +" " " normalized))
    ;; Remove punctuation at end
    (setq normalized (replace-regexp-in-string "[.!?]+$" "" normalized))
    normalized))

;;; Cache Management

(defun meta-log-llm-cache-evict-lru ()
  "Evict least recently used entry from cache."
  (let ((oldest-key nil)
        (oldest-time nil))

    ;; Find oldest entry
    (maphash (lambda (key entry)
               (let ((last-used (meta-log-llm-translation-cache-last-used entry)))
                 (when (or (null oldest-time)
                          (time-less-p last-used oldest-time))
                   (setq oldest-key key)
                   (setq oldest-time last-used))))
            meta-log-llm--cache)

    ;; Remove it
    (when oldest-key
      (remhash oldest-key meta-log-llm--cache))))

(defun meta-log-llm-cache-clear ()
  "Clear all cached translations."
  (interactive)
  (when (or (not (called-interactively-p 'any))
            (y-or-n-p "Clear all cached query translations? "))
    (clrhash meta-log-llm--cache)
    (setq meta-log-llm--cache-stats
          (list :hits 0 :misses 0 :local-count 0 :remote-count 0))
    (message "Cache cleared")))

(defun meta-log-llm-cache-remove (query)
  "Remove QUERY from cache."
  (interactive "sQuery to remove: ")
  (let ((normalized (meta-log-llm-cache--normalize-query query)))
    (if (remhash normalized meta-log-llm--cache)
        (message "Removed '%s' from cache" query)
      (message "Query not found in cache"))))

;;; Statistics

(defun meta-log-llm-cache-stats ()
  "Return cache statistics as plist."
  (let ((hits (plist-get meta-log-llm--cache-stats :hits))
        (misses (plist-get meta-log-llm--cache-stats :misses)))
    (list :hits hits
          :misses misses
          :size (hash-table-count meta-log-llm--cache)
          :hit-rate (if (> (+ hits misses) 0)
                       (/ (float hits) (+ hits misses))
                     0.0)
          :local-count (plist-get meta-log-llm--cache-stats :local-count)
          :remote-count (plist-get meta-log-llm--cache-stats :remote-count))))

(defun meta-log-llm-cache-show-stats ()
  "Display cache statistics."
  (interactive)
  (let ((stats (meta-log-llm-cache-stats)))
    (message "Cache: %d entries, %d hits, %d misses (%.1f%% hit rate)"
            (plist-get stats :size)
            (plist-get stats :hits)
            (plist-get stats :misses)
            (* 100 (plist-get stats :hit-rate)))))

;;; Top Queries

(defun meta-log-llm-cache-top-queries (&optional n)
  "Return top N most frequently cached queries.
Default N is 10."
  (interactive "p")
  (setq n (or n 10))

  (let ((queries '()))
    ;; Collect all entries
    (maphash (lambda (key entry)
               (push (cons key entry) queries))
            meta-log-llm--cache)

    ;; Sort by usage count
    (setq queries
          (sort queries
                (lambda (a b)
                  (> (meta-log-llm-translation-cache-usage-count (cdr a))
                     (meta-log-llm-translation-cache-usage-count (cdr b))))))

    ;; Take top N
    (cl-subseq queries 0 (min n (length queries)))))

(defun meta-log-llm-cache-show-top-queries ()
  "Display top queries."
  (interactive)
  (let ((top (meta-log-llm-cache-top-queries 20)))
    (with-output-to-temp-buffer "*meta-log LLM Top Queries*"
      (princ "╔══════════════════════════════════════════════════════════╗\n")
      (princ "║  TOP CACHED QUERIES                                     ║\n")
      (princ "╚══════════════════════════════════════════════════════════╝\n\n")

      (let ((i 1))
        (dolist (item top)
          (let ((query (car item))
                (entry (cdr item))
                (usage (meta-log-llm-translation-cache-usage-count (cdr item))))
            (princ (format "%2d. [%3d uses] %s\n" i usage query))
            (setq i (1+ i))))))))

;;; Persistence

(defun meta-log-llm-cache-save ()
  "Save cache to file."
  (interactive)

  (with-temp-file meta-log-llm-cache-file
    (insert ";;; meta-log-llm-cache.el --- Persisted query cache\n\n")
    (insert ";; This file is automatically generated\n")
    (insert (format ";; Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))

    (insert "(setq meta-log-llm--cache-data\n")
    (insert "  '(\n")

    ;; Write each cache entry
    (maphash (lambda (key entry)
               (let ((result (meta-log-llm-translation-cache-result entry)))
                 (insert (format "    (%S\n" key))
                 (insert (format "     :translated-query %S\n"
                               (meta-log-llm-query-result-translated-query result)))
                 (insert (format "     :expanded-concepts %S\n"
                               (meta-log-llm-query-result-expanded-concepts result)))
                 (insert (format "     :dimension %S\n"
                               (meta-log-llm-query-result-dimension result)))
                 (insert (format "     :confidence %S\n"
                               (meta-log-llm-query-result-confidence result)))
                 (insert (format "     :usage-count %d\n"
                               (meta-log-llm-translation-cache-usage-count entry)))
                 (insert "    )\n")))
            meta-log-llm--cache)

    (insert "  ))\n\n")
    (insert "(provide 'meta-log-llm-cache-data)\n"))

  (message "Saved %d cached queries to %s"
          (hash-table-count meta-log-llm--cache)
          meta-log-llm-cache-file))

(defun meta-log-llm-cache-load ()
  "Load cache from file."
  (interactive)

  (when (file-exists-p meta-log-llm-cache-file)
    (load meta-log-llm-cache-file t t)

    (when (boundp 'meta-log-llm--cache-data)
      (clrhash meta-log-llm--cache)

      (dolist (entry meta-log-llm--cache-data)
        (let* ((query (car entry))
               (plist (cdr entry))
               (result (make-meta-log-llm-query-result
                       :original-query query
                       :translated-query (plist-get plist :translated-query)
                       :expanded-concepts (plist-get plist :expanded-concepts)
                       :dimension (plist-get plist :dimension)
                       :confidence (plist-get plist :confidence)
                       :source 'cache
                       :timestamp (current-time)))
               (cache-entry (make-meta-log-llm-translation-cache
                            :query query
                            :result result
                            :usage-count (or (plist-get plist :usage-count) 1)
                            :last-used (current-time)
                            :success-rate 1.0)))

          (puthash query cache-entry meta-log-llm--cache)))

      (message "Loaded %d cached queries from %s"
              (hash-table-count meta-log-llm--cache)
              meta-log-llm-cache-file))))

;;; Auto-save

(defun meta-log-llm-cache-auto-save ()
  "Auto-save cache periodically."
  (meta-log-llm-cache-save))

;; Auto-save cache when Emacs exits
(add-hook 'kill-emacs-hook #'meta-log-llm-cache-auto-save)

;;; Pattern Analysis

(defun meta-log-llm-cache-find-similar (query &optional threshold)
  "Find cached queries similar to QUERY.
THRESHOLD is minimum similarity (0.0-1.0), default 0.7."
  (setq threshold (or threshold 0.7))

  (let ((similar '())
        (query-words (split-string (downcase query) "\\W+" t)))

    (maphash (lambda (key entry)
               (let* ((cached-words (split-string key "\\W+" t))
                      (common (cl-intersection query-words cached-words :test #'string=))
                      (similarity (/ (float (length common))
                                   (max (length query-words) (length cached-words)))))
                 (when (>= similarity threshold)
                   (push (list :query key
                              :similarity similarity
                              :entry entry)
                         similar))))
            meta-log-llm--cache)

    ;; Sort by similarity
    (sort similar (lambda (a b)
                   (> (plist-get a :similarity)
                      (plist-get b :similarity))))))

(provide 'meta-log-llm-cache)

;;; meta-log-llm-cache.el ends here
