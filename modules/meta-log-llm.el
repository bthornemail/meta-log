;;; meta-log-llm.el --- LLM integration for query translation and reasoning

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Hybrid LLM system for meta-log:
;; - Translates indefinite concepts to structured queries
;; - Multi-tier approach: Cache → Local embeddings → Remote API
;; - Learns from user queries over time
;; - Integrates with knowledge graph and federation

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;;; Configuration

(defgroup meta-log-llm nil
  "LLM integration for meta-log query translation."
  :group 'meta-log
  :prefix "meta-log-llm-")

(defcustom meta-log-llm-backend 'hybrid
  "LLM backend to use.
Options: 'hybrid (recommended), 'anthropic, 'openai, 'tflite, 'ollama"
  :type '(choice (const :tag "Hybrid (Cache + Local + Remote)" hybrid)
                 (const :tag "Claude API only" anthropic)
                 (const :tag "OpenAI API only" openai)
                 (const :tag "TensorFlow Lite only" tflite)
                 (const :tag "Ollama only" ollama))
  :group 'meta-log-llm)

(defcustom meta-log-llm-enable-learning t
  "Enable learning from query patterns."
  :type 'boolean
  :group 'meta-log-llm)

(defcustom meta-log-llm-enable-federation-sharing nil
  "Share learned query patterns via federation (privacy-preserving)."
  :type 'boolean
  :group 'meta-log-llm)

(defcustom meta-log-llm-confidence-threshold 0.7
  "Minimum confidence to use local results (0.0-1.0).
If confidence below threshold, fall back to remote API."
  :type 'float
  :group 'meta-log-llm)

;;; Data Structures

(cl-defstruct meta-log-llm-query-result
  "Result of LLM query translation."
  original-query      ; User's natural language query
  translated-query    ; Structured Prolog/Datalog query
  expanded-concepts   ; List of related concepts discovered
  dimension          ; Suggested dimension(s)
  confidence         ; Confidence score (0.0-1.0)
  source             ; 'cache, 'local, or 'remote
  timestamp          ; When translation occurred
  metadata)          ; Additional metadata

(cl-defstruct meta-log-llm-translation-cache
  "Cache entry for query translation."
  query              ; Original query (key)
  result             ; meta-log-llm-query-result
  usage-count        ; How many times used
  last-used          ; Last access timestamp
  success-rate)      ; How often this translation succeeded

;;; Backend Registry

(defvar meta-log-llm--backends (make-hash-table :test 'eq)
  "Registry of available LLM backends.")

(defun meta-log-llm-register-backend (name translate-fn expand-fn)
  "Register LLM backend NAME with TRANSLATE-FN and EXPAND-FN."
  (puthash name
           (list :translate translate-fn
                 :expand expand-fn)
           meta-log-llm--backends))

(defun meta-log-llm-backend-available-p (backend)
  "Check if BACKEND is available."
  (gethash backend meta-log-llm--backends))

;;; Core Query Translation Interface

(defun meta-log-llm-query (natural-language-query)
  "Translate NATURAL-LANGUAGE-QUERY to structured query.
Returns meta-log-llm-query-result."
  (interactive "sQuery: ")

  (let ((result (meta-log-llm--translate-with-fallback natural-language-query)))

    ;; Learn from this query if enabled
    (when meta-log-llm-enable-learning
      (meta-log-llm-learn-from-query natural-language-query result))

    ;; Display results if interactive
    (when (called-interactively-p 'any)
      (meta-log-llm--display-result result))

    result))

(defun meta-log-llm--translate-with-fallback (query)
  "Translate QUERY using fallback chain."
  (or
   ;; Tier 1: Check cache
   (meta-log-llm-cache-lookup query)

   ;; Tier 2: Try local processing
   (and (memq meta-log-llm-backend '(hybrid tflite))
        (let ((local-result (meta-log-llm-translate-local query)))
          (if (>= (meta-log-llm-query-result-confidence local-result)
                 meta-log-llm-confidence-threshold)
              local-result
            nil)))

   ;; Tier 3: Remote API
   (and (memq meta-log-llm-backend '(hybrid anthropic openai ollama))
        (meta-log-llm-translate-remote query))

   ;; Fallback: Return nil with error
   (make-meta-log-llm-query-result
    :original-query query
    :translated-query nil
    :expanded-concepts nil
    :confidence 0.0
    :source 'failed
    :timestamp (current-time))))

;;; Local Translation (TFLite embeddings)

(defun meta-log-llm-translate-local (query)
  "Translate QUERY using local TFLite embeddings.
Returns meta-log-llm-query-result."
  (require 'meta-log-llm-tflite)
  (meta-log-llm-tflite-translate query))

;;; Remote Translation (API backends)

(defun meta-log-llm-translate-remote (query)
  "Translate QUERY using remote API.
Chooses backend based on meta-log-llm-backend."
  (pcase meta-log-llm-backend
    ('anthropic
     (require 'meta-log-llm-anthropic)
     (meta-log-llm-anthropic-translate query))

    ('openai
     (require 'meta-log-llm-openai)
     (meta-log-llm-openai-translate query))

    ('ollama
     (require 'meta-log-llm-ollama)
     (meta-log-llm-ollama-translate query))

    ('hybrid
     ;; In hybrid mode, prefer Claude
     (if (meta-log-llm-backend-available-p 'anthropic)
         (progn
           (require 'meta-log-llm-anthropic)
           (meta-log-llm-anthropic-translate query))
       (progn
         (require 'meta-log-llm-openai)
         (meta-log-llm-openai-translate query))))

    (_ (error "Unknown backend: %s" meta-log-llm-backend))))

;;; Concept Expansion

(defun meta-log-llm-expand-concept (concept)
  "Expand CONCEPT to related terms and synonyms.
Returns list of related concepts."
  (interactive "sConcept: ")

  (let* ((backend (or (and (eq meta-log-llm-backend 'hybrid) 'tflite)
                     meta-log-llm-backend))
         (backend-spec (gethash backend meta-log-llm--backends))
         (expand-fn (plist-get backend-spec :expand)))

    (if expand-fn
        (funcall expand-fn concept)
      ;; Fallback to WordNet
      (require 'meta-log-wordnet)
      (meta-log-wordnet-find-synonyms concept))))

;;; Dimension Classification

(defun meta-log-llm-classify-dimension (concept)
  "Classify CONCEPT into dimensional topology (0D-7D).
Returns plist with :dimension, :confidence, :reasoning."
  (interactive "sConcept: ")

  ;; Build prompt for LLM
  (let* ((query (format "What dimension is '%s'?" concept))
         (result (meta-log-llm-query query)))

    (list :dimension (meta-log-llm-query-result-dimension result)
          :confidence (meta-log-llm-query-result-confidence result)
          :reasoning (plist-get (meta-log-llm-query-result-metadata result) :reasoning))))

;;; Query to Prolog/Datalog Translation

(defun meta-log-llm-to-prolog (natural-language-query)
  "Translate NATURAL-LANGUAGE-QUERY to Prolog query string.
Returns Prolog query or nil if translation failed."
  (let ((result (meta-log-llm-query natural-language-query)))
    (meta-log-llm-query-result-translated-query result)))

(defun meta-log-llm-to-datalog (natural-language-query)
  "Translate NATURAL-LANGUAGE-QUERY to Datalog query string.
Returns Datalog query or nil if translation failed."
  (let* ((prolog (meta-log-llm-to-prolog natural-language-query)))
    ;; Convert Prolog to Datalog format
    (when prolog
      (meta-log-llm--prolog-to-datalog prolog))))

(defun meta-log-llm--prolog-to-datalog (prolog-query)
  "Convert PROLOG-QUERY to Datalog format."
  ;; Simple conversion - can be enhanced
  (replace-regexp-in-string "\\?" "" prolog-query))

;;; Learning System

(defun meta-log-llm-learn-from-query (query result)
  "Learn from successful QUERY translation RESULT."
  (when meta-log-llm-enable-learning
    (require 'meta-log-llm-cache)
    (meta-log-llm-cache-store query result)

    (require 'meta-log-llm-learning)
    (meta-log-llm-learning-record-pattern query result)))

;;; Personal Vocabulary

(defvar meta-log-llm--personal-vocabulary (make-hash-table :test 'equal)
  "User's personal vocabulary mappings.")

(defun meta-log-llm-add-vocabulary (term canonical-form)
  "Add personal vocabulary: TERM maps to CANONICAL-FORM.
Example: 'my encoding stuff' → 'church encoding'"
  (interactive "sTerm: \nsCanonical form: ")
  (puthash (downcase term) canonical-form meta-log-llm--personal-vocabulary)
  (message "Added vocabulary: '%s' → '%s'" term canonical-form))

(defun meta-log-llm-apply-vocabulary (query)
  "Apply personal vocabulary to QUERY.
Returns transformed query with personal terms replaced."
  (let ((transformed query))
    (maphash (lambda (term canonical)
               (setq transformed
                     (replace-regexp-in-string
                      (regexp-quote term) canonical transformed t t)))
            meta-log-llm--personal-vocabulary)
    transformed))

;;; Prompt Engineering

(defun meta-log-llm-build-translation-prompt (query)
  "Build prompt for translating QUERY to Prolog/Datalog."
  (format "You are a query translator for a dimensional knowledge graph system.

The system uses a 0D-7D dimensional topology:
- 0D: Topology, identity, points, static data
- 1D: Temporal, time, sequences, streams
- 2D: Structure, spatial, pairs, graphs
- 3D: Algebraic, arithmetic, operations, composition
- 4D: Network, distributed systems, spacetime
- 5D: Consensus, blockchain, shared state, agreement
- 6D: Intelligence, AI, neural systems, learning
- 7D: Quantum, superposition, meta-circular

Natural language query:
\"%s\"

Tasks:
1. Determine the most relevant dimension(s)
2. Expand the concept to related terms
3. Generate a Prolog query for the knowledge graph

Available predicates:
- kg_node(Id, Type, Label): Match nodes by type and label
- kg_dimension(Id, Dimension): Filter by dimension
- kg_relation(From, Relation, To): Follow relationships
- kg_semantic_field(Id, Field): Match semantic field
- kg_property(Id, Property, Value): Match property values

Return JSON:
{
  \"dimension\": \"XD\",
  \"concepts\": [\"concept1\", \"concept2\"],
  \"prolog\": \"kg_node(?Id, ?Type, ?Label), ...\",
  \"confidence\": 0.95,
  \"reasoning\": \"explanation\"
}" query))

(defun meta-log-llm-build-expansion-prompt (concept)
  "Build prompt for expanding CONCEPT to related terms."
  (format "Given the concept: \"%s\"

In the context of a dimensional knowledge graph with 0D-7D topology,
provide related concepts, synonyms, and terms that would help find
relevant information.

Return JSON array:
[\"related1\", \"related2\", \"related3\", ...]

Focus on: technical terms, synonyms, related concepts, dimensional context." concept))

;;; Result Display

(defun meta-log-llm--display-result (result)
  "Display RESULT of query translation."
  (with-output-to-temp-buffer "*meta-log LLM Result*"
    (princ "╔══════════════════════════════════════════════════════════╗\n")
    (princ "║  LLM QUERY TRANSLATION RESULT                           ║\n")
    (princ "╚══════════════════════════════════════════════════════════╝\n\n")

    (princ (format "Original Query: %s\n\n"
                   (meta-log-llm-query-result-original-query result)))

    (princ (format "Translated Query:\n%s\n\n"
                   (or (meta-log-llm-query-result-translated-query result)
                       "  (translation failed)")))

    (let ((concepts (meta-log-llm-query-result-expanded-concepts result)))
      (when concepts
        (princ "Expanded Concepts:\n")
        (dolist (concept concepts)
          (princ (format "  • %s\n" concept)))
        (princ "\n")))

    (princ (format "Dimension: %s\n"
                   (or (meta-log-llm-query-result-dimension result) "unknown")))

    (princ (format "Confidence: %.2f\n"
                   (meta-log-llm-query-result-confidence result)))

    (princ (format "Source: %s\n"
                   (meta-log-llm-query-result-source result)))

    (let ((reasoning (plist-get (meta-log-llm-query-result-metadata result) :reasoning)))
      (when reasoning
        (princ (format "\nReasoning:\n%s\n" reasoning))))))

;;; Statistics

(defun meta-log-llm-stats ()
  "Show LLM usage statistics."
  (interactive)
  (require 'meta-log-llm-cache)
  (let ((cache-stats (meta-log-llm-cache-stats)))
    (with-output-to-temp-buffer "*meta-log LLM Stats*"
      (princ "╔══════════════════════════════════════════════════════════╗\n")
      (princ "║  LLM USAGE STATISTICS                                   ║\n")
      (princ "╚══════════════════════════════════════════════════════════╝\n\n")

      (princ (format "Cache hits: %d\n" (plist-get cache-stats :hits)))
      (princ (format "Cache misses: %d\n" (plist-get cache-stats :misses)))
      (princ (format "Cache size: %d entries\n" (plist-get cache-stats :size)))
      (princ (format "Hit rate: %.1f%%\n\n"
                    (* 100 (plist-get cache-stats :hit-rate))))

      (princ "Backend usage:\n")
      (princ (format "  Local (TFLite): %d queries\n"
                    (plist-get cache-stats :local-count)))
      (princ (format "  Remote (API): %d queries\n"
                    (plist-get cache-stats :remote-count))))))

;;; Initialization

(defun meta-log-llm-initialize ()
  "Initialize LLM system."
  (interactive)

  ;; Load cache
  (require 'meta-log-llm-cache)
  (meta-log-llm-cache-load)

  ;; Load personal vocabulary
  (meta-log-llm--load-vocabulary)

  ;; Register available backends
  (when (meta-log-llm-backend-available-p 'tflite)
    (require 'meta-log-llm-tflite))

  (message "meta-log LLM system initialized (backend: %s)" meta-log-llm-backend))

(defun meta-log-llm--load-vocabulary ()
  "Load personal vocabulary from file."
  (let ((vocab-file (expand-file-name "meta-log-vocabulary.el" user-emacs-directory)))
    (when (file-exists-p vocab-file)
      (load vocab-file t t))))

(defun meta-log-llm-save-vocabulary ()
  "Save personal vocabulary to file."
  (interactive)
  (let ((vocab-file (expand-file-name "meta-log-vocabulary.el" user-emacs-directory)))
    (with-temp-file vocab-file
      (insert ";;; meta-log-vocabulary.el --- Personal vocabulary mappings\n\n")
      (insert ";; This file is automatically generated\n\n")
      (insert "(setq meta-log-llm--personal-vocabulary\n")
      (insert "  (let ((vocab (make-hash-table :test 'equal)))\n")
      (maphash (lambda (term canonical)
                 (insert (format "    (puthash %S %S vocab)\n" term canonical)))
              meta-log-llm--personal-vocabulary)
      (insert "    vocab))\n"))
    (message "Saved personal vocabulary to %s" vocab-file)))

(provide 'meta-log-llm)

;;; meta-log-llm.el ends here
