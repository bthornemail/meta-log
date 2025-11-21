;;; meta-log-llm-learning.el --- Learn from query patterns and user behavior

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Learning system that improves over time:
;; - Tracks successful query patterns
;; - Learns user vocabulary and preferences
;; - Builds personal knowledge base
;; - Shares learned patterns via federation (optional)

;;; Code:

(require 'cl-lib)
(require 'meta-log-llm-cache)

;;; Configuration

(defcustom meta-log-llm-learning-file
  (expand-file-name "meta-log-llm-learning.el" user-emacs-directory)
  "File to persist learned patterns."
  :type 'file
  :group 'meta-log-llm)

;;; Data Structures

(defvar meta-log-llm--query-patterns (make-hash-table :test 'equal)
  "Learned query patterns.
Key: pattern signature
Value: plist with :count, :success-rate, :translations")

(defvar meta-log-llm--concept-associations (make-hash-table :test 'equal)
  "Learned concept associations.
Key: concept
Value: list of (related-concept . strength)")

(defvar meta-log-llm--user-corrections (make-hash-table :test 'equal)
  "User corrections to translations.
Key: original query
Value: corrected translation")

;;; Pattern Recording

(defun meta-log-llm-learning-record-pattern (query result)
  "Record successful query pattern: QUERY → RESULT."
  (let* ((pattern (meta-log-llm-learning--extract-pattern query))
         (existing (gethash pattern meta-log-llm--query-patterns))
         (translation (meta-log-llm-query-result-translated-query result)))

    (if existing
        ;; Update existing pattern
        (let ((count (plist-get existing :count))
              (translations (plist-get existing :translations)))
          (puthash pattern
                  (list :count (1+ count)
                        :success-rate (plist-get existing :success-rate)
                        :translations (cons translation translations))
                  meta-log-llm--query-patterns))

      ;; New pattern
      (puthash pattern
              (list :count 1
                    :success-rate 1.0
                    :translations (list translation))
              meta-log-llm--query-patterns))

    ;; Learn concept associations
    (meta-log-llm-learning--learn-associations query result)))

(defun meta-log-llm-learning--extract-pattern (query)
  "Extract abstract pattern from QUERY.
Example: 'church encoding' → 'CONCEPT in DIMENSION'"
  (let ((words (split-string (downcase query) "\\W+" t)))
    ;; Simple pattern: just join key words
    (mapconcat #'identity
               (cl-remove-if (lambda (w) (member w '("the" "a" "an" "in" "of" "for" "with")))
                           words)
               " ")))

(defun meta-log-llm-learning--learn-associations (query result)
  "Learn concept associations from QUERY and RESULT."
  (let ((concepts (meta-log-llm-query-result-expanded-concepts result)))
    (dolist (concept1 concepts)
      (dolist (concept2 concepts)
        (unless (string= concept1 concept2)
          (meta-log-llm-learning--add-association concept1 concept2))))))

(defun meta-log-llm-learning--add-association (concept1 concept2)
  "Add association between CONCEPT1 and CONCEPT2."
  (let ((existing (gethash concept1 meta-log-llm--concept-associations)))
    (if existing
        ;; Increment strength
        (let ((assoc (assoc concept2 existing)))
          (if assoc
              (setcdr assoc (1+ (cdr assoc)))
            (push (cons concept2 1) existing)
            (puthash concept1 existing meta-log-llm--concept-associations)))

      ;; New association
      (puthash concept1 (list (cons concept2 1))
              meta-log-llm--concept-associations))))

;;; User Corrections

(defun meta-log-llm-learning-correct-translation (query correct-translation)
  "Record user correction: QUERY should translate to CORRECT-TRANSLATION."
  (interactive "sOriginal query: \nsCorrect translation: ")

  (puthash query correct-translation meta-log-llm--user-corrections)

  ;; Update cache with corrected translation
  (let ((result (make-meta-log-llm-query-result
                :original-query query
                :translated-query correct-translation
                :confidence 1.0
                :source 'user-corrected
                :timestamp (current-time))))

    (require 'meta-log-llm-cache)
    (meta-log-llm-cache-store query result))

  (message "Correction recorded for: %s" query))

;;; Pattern Suggestions

(defun meta-log-llm-learning-suggest-translation (query)
  "Suggest translation for QUERY based on learned patterns.
Returns suggested translation or nil."
  (let* ((pattern (meta-log-llm-learning--extract-pattern query))
         (pattern-data (gethash pattern meta-log-llm--query-patterns)))

    (when pattern-data
      ;; Return most common translation
      (let ((translations (plist-get pattern-data :translations)))
        (car translations)))))

(defun meta-log-llm-learning-find-related-concepts (concept)
  "Find concepts related to CONCEPT based on learned associations.
Returns list of (concept . strength) sorted by strength."
  (let ((associations (gethash concept meta-log-llm--concept-associations)))
    (when associations
      (sort (copy-sequence associations)
            (lambda (a b) (> (cdr a) (cdr b)))))))

;;; Personal Vocabulary Learning

(defun meta-log-llm-learning-learn-vocabulary (query result)
  "Learn user's vocabulary from QUERY and successful RESULT."
  (require 'meta-log-llm)

  ;; If query uses uncommon terms that mapped to common concepts,
  ;; add to personal vocabulary
  (let ((user-terms (split-string (downcase query) "\\W+" t))
        (concepts (meta-log-llm-query-result-expanded-concepts result)))

    (dolist (term user-terms)
      (when (and (> (length term) 3)  ; Skip short words
                (not (member term concepts)))  ; Term not in concepts
        ;; This might be user's personal term for a concept
        (when concepts
          (let ((canonical (car concepts)))
            (meta-log-llm-add-vocabulary term canonical)))))))

;;; Statistics and Analysis

(defun meta-log-llm-learning-stats ()
  "Show learning statistics."
  (interactive)

  (let ((pattern-count (hash-table-count meta-log-llm--query-patterns))
        (association-count (hash-table-count meta-log-llm--concept-associations))
        (correction-count (hash-table-count meta-log-llm--user-corrections)))

    (with-output-to-temp-buffer "*meta-log LLM Learning*"
      (princ "╔══════════════════════════════════════════════════════════╗\n")
      (princ "║  LEARNING SYSTEM STATISTICS                             ║\n")
      (princ "╚══════════════════════════════════════════════════════════╝\n\n")

      (princ (format "Query patterns learned: %d\n" pattern-count))
      (princ (format "Concept associations: %d\n" association-count))
      (princ (format "User corrections: %d\n\n" correction-count))

      (when (> pattern-count 0)
        (princ "Top Query Patterns:\n")
        (princ "===================\n")
        (let ((patterns '()))
          (maphash (lambda (pattern data)
                     (push (cons pattern data) patterns))
                  meta-log-llm--query-patterns)

          (setq patterns (sort patterns
                              (lambda (a b)
                                (> (plist-get (cdr a) :count)
                                   (plist-get (cdr b) :count)))))

          (dotimes (i (min 10 (length patterns)))
            (let* ((item (nth i patterns))
                   (pattern (car item))
                   (data (cdr item))
                   (count (plist-get data :count)))
              (princ (format "  %2d. [%3d uses] %s\n" (1+ i) count pattern)))))

        (princ "\n"))

      (when (> association-count 0)
        (princ "Top Concept Associations:\n")
        (princ "=========================\n")
        (let ((concepts '()))
          (maphash (lambda (concept assocs)
                     (let ((total-strength (cl-reduce #'+ assocs :key #'cdr)))
                       (push (list concept total-strength assocs) concepts)))
                  meta-log-llm--concept-associations)

          (setq concepts (sort concepts (lambda (a b) (> (cadr a) (cadr b)))))

          (dotimes (i (min 5 (length concepts)))
            (let* ((item (nth i concepts))
                   (concept (car item))
                   (strength (cadr item))
                   (assocs (caddr item)))
              (princ (format "  %s (strength: %d)\n" concept strength))
              (dolist (related (cl-subseq assocs 0 (min 3 (length assocs))))
                (princ (format "    → %s (%d)\n" (car related) (cdr related)))))))))))

;;; Persistence

(defun meta-log-llm-learning-save ()
  "Save learned patterns to file."
  (interactive)

  (with-temp-file meta-log-llm-learning-file
    (insert ";;; meta-log-llm-learning.el --- Learned patterns\n\n")
    (insert ";; This file is automatically generated\n")
    (insert (format ";; Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))

    ;; Save query patterns
    (insert "(setq meta-log-llm--query-patterns-data\n  '(\n")
    (maphash (lambda (pattern data)
               (insert (format "    (%S %S)\n" pattern data)))
            meta-log-llm--query-patterns)
    (insert "  ))\n\n")

    ;; Save concept associations
    (insert "(setq meta-log-llm--concept-associations-data\n  '(\n")
    (maphash (lambda (concept assocs)
               (insert (format "    (%S %S)\n" concept assocs)))
            meta-log-llm--concept-associations)
    (insert "  ))\n\n")

    ;; Save user corrections
    (insert "(setq meta-log-llm--user-corrections-data\n  '(\n")
    (maphash (lambda (query translation)
               (insert (format "    (%S . %S)\n" query translation)))
            meta-log-llm--user-corrections)
    (insert "  ))\n\n")

    (insert "(provide 'meta-log-llm-learning-data)\n"))

  (message "Saved learning data to %s" meta-log-llm-learning-file))

(defun meta-log-llm-learning-load ()
  "Load learned patterns from file."
  (when (file-exists-p meta-log-llm-learning-file)
    (load meta-log-llm-learning-file t t)

    ;; Restore query patterns
    (when (boundp 'meta-log-llm--query-patterns-data)
      (clrhash meta-log-llm--query-patterns)
      (dolist (item meta-log-llm--query-patterns-data)
        (puthash (car item) (cadr item) meta-log-llm--query-patterns)))

    ;; Restore concept associations
    (when (boundp 'meta-log-llm--concept-associations-data)
      (clrhash meta-log-llm--concept-associations)
      (dolist (item meta-log-llm--concept-associations-data)
        (puthash (car item) (cadr item) meta-log-llm--concept-associations)))

    ;; Restore user corrections
    (when (boundp 'meta-log-llm--user-corrections-data)
      (clrhash meta-log-llm--user-corrections)
      (dolist (item meta-log-llm--user-corrections-data)
        (puthash (car item) (cdr item) meta-log-llm--user-corrections)))

    (message "Loaded learning data from %s" meta-log-llm-learning-file)))

;;; Federation Integration

(defun meta-log-llm-learning-export-patterns ()
  "Export learned patterns for federation sharing.
Returns privacy-preserving pattern data."
  (let ((patterns '()))
    (maphash (lambda (pattern data)
               ;; Only share patterns used 3+ times (more reliable)
               (when (>= (plist-get data :count) 3)
                 (push (list :pattern pattern
                           :success-rate (plist-get data :success-rate)
                           ;; Don't share actual translations (privacy)
                           :usage-count (plist-get data :count))
                      patterns)))
            meta-log-llm--query-patterns)
    patterns))

(defun meta-log-llm-learning-import-patterns (patterns)
  "Import PATTERNS from federation.
Merges with local patterns."
  (dolist (pattern-data patterns)
    (let ((pattern (plist-get pattern-data :pattern))
          (count (plist-get pattern-data :usage-count))
          (success-rate (plist-get pattern-data :success-rate)))

      ;; Only import high-quality patterns
      (when (and (>= count 5) (>= success-rate 0.8))
        (let ((existing (gethash pattern meta-log-llm--query-patterns)))
          (unless existing
            ;; Add as new learned pattern
            (puthash pattern
                    (list :count 1  ; Start at 1 locally
                          :success-rate success-rate
                          :translations '())
                    meta-log-llm--query-patterns)))))))

;;; Auto-save

(add-hook 'kill-emacs-hook #'meta-log-llm-learning-save)

(provide 'meta-log-llm-learning)

;;; meta-log-llm-learning.el ends here
