;;; meta-log-llm-anthropic.el --- Claude API integration via Model Context Protocol

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Claude API integration using Anthropic's Messages API.
;; Supports Model Context Protocol (MCP) for enhanced capabilities.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;;; Configuration

(defcustom meta-log-llm-anthropic-api-key nil
  "Anthropic API key for Claude."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "API Key"))
  :group 'meta-log-llm)

(defcustom meta-log-llm-anthropic-model "claude-3-5-haiku-20241022"
  "Claude model to use.
Options:
- claude-3-5-sonnet-20241022 (most capable)
- claude-3-5-haiku-20241022 (fast, cheap - recommended)
- claude-3-opus-20240229 (legacy, most capable)"
  :type 'string
  :group 'meta-log-llm)

(defcustom meta-log-llm-anthropic-max-tokens 1024
  "Maximum tokens in response."
  :type 'integer
  :group 'meta-log-llm)

(defcustom meta-log-llm-anthropic-temperature 0.3
  "Temperature for generation (0.0-1.0)."
  :type 'float
  :group 'meta-log-llm)

(defconst meta-log-llm-anthropic-api-version "2023-06-01"
  "Anthropic API version.")

;;; API Client

(defun meta-log-llm-anthropic-translate (query)
  "Translate QUERY using Claude API.
Returns meta-log-llm-query-result."
  (unless meta-log-llm-anthropic-api-key
    (error "Anthropic API key not set. Set meta-log-llm-anthropic-api-key"))

  (let* ((prompt (meta-log-llm-build-translation-prompt query))
         (response (meta-log-llm-anthropic--messages prompt))
         (content (meta-log-llm-anthropic--extract-content response)))

    (meta-log-llm-anthropic--parse-translation query content)))

(defun meta-log-llm-anthropic-expand (concept)
  "Expand CONCEPT using Claude API.
Returns list of related concepts."
  (unless meta-log-llm-anthropic-api-key
    (error "Anthropic API key not set"))

  (let* ((prompt (meta-log-llm-build-expansion-prompt concept))
         (response (meta-log-llm-anthropic--messages prompt))
         (content (meta-log-llm-anthropic--extract-content response)))

    (meta-log-llm-anthropic--parse-expansion content)))

;;; Messages API

(defun meta-log-llm-anthropic--messages (prompt &optional system-prompt)
  "Send message to Claude with PROMPT.
SYSTEM-PROMPT is optional system message.
Returns parsed JSON response."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("x-api-key" . ,meta-log-llm-anthropic-api-key)
            ("anthropic-version" . ,meta-log-llm-anthropic-api-version)))
         (url-request-data
          (encode-coding-string
           (json-encode
            `((model . ,meta-log-llm-anthropic-model)
              (max_tokens . ,meta-log-llm-anthropic-max-tokens)
              (temperature . ,meta-log-llm-anthropic-temperature)
              ,@(when system-prompt
                  `((system . ,system-prompt)))
              (messages . [((role . "user")
                           (content . ,prompt))])))
           'utf-8))
         (url "https://api.anthropic.com/v1/messages")
         (response-buffer nil))

    (message "Querying Claude: %s..." (substring prompt 0 (min 50 (length prompt))))

    (condition-case err
        (progn
          (setq response-buffer
                (url-retrieve-synchronously url t nil 30))

          (when response-buffer
            (with-current-buffer response-buffer
              ;; Skip HTTP headers
              (goto-char (point-min))
              (re-search-forward "^$" nil t)
              (forward-char 1)

              ;; Parse JSON response
              (let ((json-object-type 'plist)
                    (json-array-type 'list)
                    (json-key-type 'keyword))
                (condition-case parse-err
                    (json-read)
                  (error
                   (message "JSON parse error: %s" parse-err)
                   (message "Response: %s" (buffer-substring-no-properties (point) (point-max)))
                   nil))))))

      (error
       (message "Claude API request failed: %s" err)
       nil))))

(defun meta-log-llm-anthropic--extract-content (response)
  "Extract content from Claude API RESPONSE."
  (when response
    (let* ((content (plist-get response :content))
           (first-block (when content (elt content 0))))
      (plist-get first-block :text))))

;;; Response Parsing

(defun meta-log-llm-anthropic--parse-translation (original-query content)
  "Parse translation CONTENT for ORIGINAL-QUERY.
Returns meta-log-llm-query-result."
  (if (not content)
      (make-meta-log-llm-query-result
       :original-query original-query
       :translated-query nil
       :expanded-concepts nil
       :confidence 0.0
       :source 'remote
       :timestamp (current-time))

    (condition-case err
        ;; Try to parse as JSON
        (let* ((json-object-type 'plist)
               (json-array-type 'list)
               (json-key-type 'keyword)
               ;; Claude sometimes wraps JSON in markdown code blocks
               (clean-content (meta-log-llm-anthropic--extract-json content))
               (parsed (json-read-from-string clean-content)))

          (make-meta-log-llm-query-result
           :original-query original-query
           :translated-query (plist-get parsed :prolog)
           :expanded-concepts (plist-get parsed :concepts)
           :dimension (plist-get parsed :dimension)
           :confidence (or (plist-get parsed :confidence) 0.95)
           :source 'remote
           :timestamp (current-time)
           :metadata (list :reasoning (plist-get parsed :reasoning))))

      (error
       ;; Fallback: extract from text
       (message "Failed to parse Claude JSON response: %s" err)
       (meta-log-llm-anthropic--parse-text-response original-query content)))))

(defun meta-log-llm-anthropic--extract-json (content)
  "Extract JSON from CONTENT, removing markdown code blocks if present."
  (if (string-match "```json\n\\(.*?\\)\n```" content)
      (match-string 1 content)
    (if (string-match "```\n\\(.*?\\)\n```" content)
        (match-string 1 content)
      content)))

(defun meta-log-llm-anthropic--parse-text-response (original-query content)
  "Parse plain text CONTENT as fallback for ORIGINAL-QUERY."
  (let ((prolog-query nil)
        (concepts '())
        (dimension nil))

    ;; Extract Prolog query
    (when (string-match "kg_\\w+([^)]+)" content)
      (setq prolog-query (match-string 0 content)))

    ;; Extract concepts array
    (when (string-match "\"concepts\":\\s-*\\[\\([^]]+\\)\\]" content)
      (setq concepts (split-string (match-string 1 content) "," t "[ \t\n\"]")))

    ;; Extract dimension
    (when (string-match "\"dimension\":\\s-*\"\\([^\"]+\\)\"" content)
      (setq dimension (match-string 1 content)))

    (make-meta-log-llm-query-result
     :original-query original-query
     :translated-query prolog-query
     :expanded-concepts concepts
     :dimension dimension
     :confidence 0.85
     :source 'remote
     :timestamp (current-time))))

(defun meta-log-llm-anthropic--parse-expansion (content)
  "Parse expansion CONTENT.
Returns list of concepts."
  (when content
    (condition-case err
        ;; Try JSON array
        (let ((json-array-type 'list)
              (clean-content (meta-log-llm-anthropic--extract-json content)))
          (json-read-from-string clean-content))

      (error
       ;; Fallback: split by commas/newlines
       (split-string content "[,\n]" t "[ \t\"\\[\\]]+")))))

;;; Model Context Protocol (MCP) Support

(defun meta-log-llm-anthropic-with-context (query context-data)
  "Query Claude with QUERY and CONTEXT-DATA from knowledge graph.
CONTEXT-DATA is plist with relevant KG information."
  (let* ((system-prompt (format "You are translating queries for a knowledge graph with:

Documents: %d
Concepts: %d
Dimensions: 0D-7D topology

Recent concepts: %s

Use this context to provide better translations."
                               (plist-get context-data :doc-count)
                               (plist-get context-data :concept-count)
                               (plist-get context-data :recent-concepts)))
         (user-prompt (meta-log-llm-build-translation-prompt query))
         (response (meta-log-llm-anthropic--messages user-prompt system-prompt))
         (content (meta-log-llm-anthropic--extract-content response)))

    (meta-log-llm-anthropic--parse-translation query content)))

;;; Configuration

(defun meta-log-llm-anthropic-set-key (api-key)
  "Set Anthropic API-KEY."
  (interactive "sAnthropic API Key: ")
  (setq meta-log-llm-anthropic-api-key api-key)
  (message "Anthropic API key set"))

;;; Testing

(defun meta-log-llm-anthropic-test ()
  "Test Claude API connection."
  (interactive)
  (unless meta-log-llm-anthropic-api-key
    (error "API key not set. Run M-x meta-log-llm-anthropic-set-key"))

  (let ((test-query "church encoding"))
    (message "Testing Claude API with query: %s" test-query)
    (let ((result (meta-log-llm-anthropic-translate test-query)))
      (if result
          (progn
            (message "✓ Claude API test successful!")
            (message "  Translated: %s"
                    (meta-log-llm-query-result-translated-query result))
            (message "  Confidence: %.2f"
                    (meta-log-llm-query-result-confidence result))
            result)
        (message "✗ Claude API test failed")
        nil))))

;;; Backend Registration

(meta-log-llm-register-backend
 'anthropic
 #'meta-log-llm-anthropic-translate
 #'meta-log-llm-anthropic-expand)

(provide 'meta-log-llm-anthropic)

;;; meta-log-llm-anthropic.el ends here
