;;; meta-log-llm-openai.el --- OpenAI-compatible API client for meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Universal OpenAI-compatible API client that works with:
;; - OpenAI (GPT-4, GPT-3.5)
;; - Ollama (Gemma, Llama, Mistral, etc.) - local
;; - Together AI, Groq, Perplexity, and others
;; - Any OpenAI-compatible endpoint

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;;; Configuration

(defcustom meta-log-llm-openai-api-key nil
  "API key for OpenAI or compatible service.
Set to nil for Ollama (local, no key needed)."
  :type '(choice (const :tag "None (for Ollama)" nil)
                 (string :tag "API Key"))
  :group 'meta-log-llm)

(defcustom meta-log-llm-openai-endpoint "http://localhost:11434/v1"
  "API endpoint. Default is Ollama local endpoint.
Examples:
- Ollama: http://localhost:11434/v1
- OpenAI: https://api.openai.com/v1
- Together: https://api.together.xyz/v1
- Groq: https://api.groq.com/openai/v1"
  :type 'string
  :group 'meta-log-llm)

(defcustom meta-log-llm-openai-model "gemma2:2b"
  "Model to use. Examples:
- Ollama: gemma2:2b, llama3.2:3b, qwen2.5:3b
- OpenAI: gpt-4o-mini, gpt-3.5-turbo
- Together: meta-llama/Llama-3-8b-chat-hf"
  :type 'string
  :group 'meta-log-llm)

(defcustom meta-log-llm-openai-temperature 0.3
  "Temperature for generation (0.0-2.0).
Lower = more focused, Higher = more creative."
  :type 'float
  :group 'meta-log-llm)

(defcustom meta-log-llm-openai-max-tokens 500
  "Maximum tokens in response."
  :type 'integer
  :group 'meta-log-llm)

;;; API Client

(defun meta-log-llm-openai-translate (query)
  "Translate QUERY using OpenAI-compatible API.
Returns meta-log-llm-query-result."
  (let* ((prompt (meta-log-llm-build-translation-prompt query))
         (response (meta-log-llm-openai--chat-completion prompt))
         (content (meta-log-llm-openai--extract-content response)))

    (meta-log-llm-openai--parse-translation query content)))

(defun meta-log-llm-openai-expand (concept)
  "Expand CONCEPT using OpenAI-compatible API.
Returns list of related concepts."
  (let* ((prompt (meta-log-llm-build-expansion-prompt concept))
         (response (meta-log-llm-openai--chat-completion prompt))
         (content (meta-log-llm-openai--extract-content response)))

    (meta-log-llm-openai--parse-expansion content)))

;;; HTTP Client

(defun meta-log-llm-openai--chat-completion (prompt)
  "Send chat completion request with PROMPT.
Returns parsed JSON response."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          (append
           '(("Content-Type" . "application/json"))
           (when meta-log-llm-openai-api-key
             `(("Authorization" . ,(format "Bearer %s" meta-log-llm-openai-api-key))))))
         (url-request-data
          (encode-coding-string
           (json-encode
            `((model . ,meta-log-llm-openai-model)
              (messages . [((role . "system")
                           (content . "You are a query translator for a dimensional knowledge graph. Always return valid JSON."))
                          ((role . "user")
                           (content . ,prompt))])
              (temperature . ,meta-log-llm-openai-temperature)
              (max_tokens . ,meta-log-llm-openai-max-tokens)))
           'utf-8))
         (url (concat meta-log-llm-openai-endpoint "/chat/completions"))
         (response-buffer nil))

    (message "Querying LLM: %s..." (substring prompt 0 (min 50 (length prompt))))

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
       (message "API request failed: %s" err)
       nil))))

(defun meta-log-llm-openai--extract-content (response)
  "Extract content from API RESPONSE."
  (when response
    (let* ((choices (plist-get response :choices))
           (first-choice (when choices (elt choices 0)))
           (message (plist-get first-choice :message)))
      (plist-get message :content))))

;;; Response Parsing

(defun meta-log-llm-openai--parse-translation (original-query content)
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
               (parsed (json-read-from-string content)))

          (make-meta-log-llm-query-result
           :original-query original-query
           :translated-query (plist-get parsed :prolog)
           :expanded-concepts (plist-get parsed :concepts)
           :dimension (plist-get parsed :dimension)
           :confidence (or (plist-get parsed :confidence) 0.9)
           :source 'remote
           :timestamp (current-time)
           :metadata (list :reasoning (plist-get parsed :reasoning))))

      (error
       ;; Fallback: extract query from text
       (message "Failed to parse JSON response: %s" err)
       (meta-log-llm-openai--parse-text-response original-query content)))))

(defun meta-log-llm-openai--parse-text-response (original-query content)
  "Parse plain text CONTENT as fallback for ORIGINAL-QUERY."
  (let ((prolog-query nil)
        (concepts '()))

    ;; Try to extract Prolog query
    (when (string-match "kg_\\w+([^)]+)" content)
      (setq prolog-query (match-string 0 content)))

    ;; Try to extract concepts
    (when (string-match "\\[\\([^]]+\\)\\]" content)
      (setq concepts (split-string (match-string 1 content) "," t "[ \t\n\"]")))

    (make-meta-log-llm-query-result
     :original-query original-query
     :translated-query prolog-query
     :expanded-concepts concepts
     :confidence 0.7
     :source 'remote
     :timestamp (current-time))))

(defun meta-log-llm-openai--parse-expansion (content)
  "Parse expansion CONTENT.
Returns list of concepts."
  (when content
    (condition-case err
        ;; Try JSON array
        (let ((json-array-type 'list))
          (json-read-from-string content))

      (error
       ;; Fallback: split by commas/newlines
       (split-string content "[,\n]" t "[ \t\"\\[\\]]+")))))

;;; Presets for Common Services

(defun meta-log-llm-openai-use-ollama (&optional model)
  "Configure for Ollama (local).
MODEL defaults to gemma2:2b"
  (interactive)
  (setq meta-log-llm-openai-endpoint "http://localhost:11434/v1"
        meta-log-llm-openai-api-key nil
        meta-log-llm-openai-model (or model "gemma2:2b"))
  (message "Configured for Ollama: %s" meta-log-llm-openai-model))

(defun meta-log-llm-openai-use-openai (api-key &optional model)
  "Configure for OpenAI with API-KEY.
MODEL defaults to gpt-4o-mini"
  (interactive "sOpenAI API Key: ")
  (setq meta-log-llm-openai-endpoint "https://api.openai.com/v1"
        meta-log-llm-openai-api-key api-key
        meta-log-llm-openai-model (or model "gpt-4o-mini"))
  (message "Configured for OpenAI: %s" meta-log-llm-openai-model))

(defun meta-log-llm-openai-use-together (api-key &optional model)
  "Configure for Together AI with API-KEY.
MODEL defaults to meta-llama/Llama-3-8b-chat-hf"
  (interactive "sTogether API Key: ")
  (setq meta-log-llm-openai-endpoint "https://api.together.xyz/v1"
        meta-log-llm-openai-api-key api-key
        meta-log-llm-openai-model (or model "meta-llama/Llama-3-8b-chat-hf"))
  (message "Configured for Together AI: %s" meta-log-llm-openai-model))

(defun meta-log-llm-openai-use-groq (api-key &optional model)
  "Configure for Groq with API-KEY.
MODEL defaults to llama-3.1-8b-instant"
  (interactive "sGroq API Key: ")
  (setq meta-log-llm-openai-endpoint "https://api.groq.com/openai/v1"
        meta-log-llm-openai-api-key api-key
        meta-log-llm-openai-model (or model "llama-3.1-8b-instant"))
  (message "Configured for Groq: %s" meta-log-llm-openai-model))

;;; Testing

(defun meta-log-llm-openai-test ()
  "Test OpenAI-compatible API connection."
  (interactive)
  (let ((test-query "church encoding"))
    (message "Testing API with query: %s" test-query)
    (let ((result (meta-log-llm-openai-translate test-query)))
      (if result
          (progn
            (message "✓ API test successful!")
            (message "  Translated: %s"
                    (meta-log-llm-query-result-translated-query result))
            (message "  Confidence: %.2f"
                    (meta-log-llm-query-result-confidence result))
            result)
        (message "✗ API test failed")
        nil))))

;;; Backend Registration

(meta-log-llm-register-backend
 'openai
 #'meta-log-llm-openai-translate
 #'meta-log-llm-openai-expand)

(provide 'meta-log-llm-openai)

;;; meta-log-llm-openai.el ends here
