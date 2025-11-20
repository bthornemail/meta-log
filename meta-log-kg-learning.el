;;; meta-log-kg-learning.el --- Learn from knowledge graph to enhance meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Use the knowledge graph to enhance meta-log itself:
;; - Learn new patterns from documents
;; - Discover missing capabilities
;; - Generate new modules based on common patterns
;; - Self-improve WordNet mappings
;; - Enhance template discovery

;;; Code:

(require 'cl-lib)
(require 'meta-log-knowledge-graph)
(require 'meta-log-metacircular)
(require 'meta-log-wordnet)
(require 'meta-log-template-discovery)

;;; Pattern Discovery from Knowledge Graph

(defun meta-log-kg-discover-patterns ()
  "Discover common patterns from the knowledge graph.
Returns list of discovered patterns."
  (interactive)

  (let ((patterns '())
        (concept-frequency (make-hash-table :test 'equal)))

    (message "Analyzing knowledge graph for patterns...")

    ;; Count concept frequencies
    (maphash (lambda (id node)
               (let ((label (downcase (meta-log-kg-node-label node))))
                 (puthash label (1+ (or (gethash label concept-frequency) 0))
                         concept-frequency)))
             meta-log-kg--concept-index)

    ;; Find frequent patterns (threshold: appears in 5+ documents)
    (maphash (lambda (label count)
               (when (>= count 5)
                 (push (list :pattern label
                           :frequency count
                           :type 'concept)
                       patterns)))
             concept-frequency)

    ;; Sort by frequency
    (setq patterns (sort patterns
                        (lambda (a b)
                          (> (plist-get a :frequency)
                             (plist-get b :frequency)))))

    (message "Discovered %d common patterns" (length patterns))
    patterns))

;;; Enhancement 1: Learn New WordNet Mappings

(defun meta-log-kg-learn-wordnet-mappings ()
  "Learn new word-to-dimension mappings from the knowledge graph.
Enhances WordNet with project-specific vocabulary."
  (interactive)

  (let ((new-mappings '())
        (dimension-words (make-hash-table :test 'equal)))

    (message "Learning new WordNet mappings from knowledge graph...")

    ;; Collect words by dimension
    (maphash (lambda (id node)
               (let ((dimension (meta-log-kg-node-dimension node))
                     (keywords (plist-get (meta-log-kg-node-properties node)
                                         :keywords)))
                 (when (and dimension keywords)
                   (dolist (word keywords)
                     (let ((word-lower (downcase word)))
                       (push dimension
                             (gethash word-lower dimension-words)))))))
             meta-log-kg--document-index)

    ;; Find words with consistent dimension mapping
    (maphash (lambda (word dimensions)
               ;; If word appears in same dimension 3+ times
               (let ((dim-counts (make-hash-table :test 'equal)))
                 (dolist (dim dimensions)
                   (puthash dim (1+ (or (gethash dim dim-counts) 0))
                           dim-counts))

                 (maphash (lambda (dim count)
                            (when (>= count 3)
                              ;; New mapping found!
                              (push (cons word dim) new-mappings)))
                         dim-counts)))
             dimension-words)

    (message "Learned %d new word-to-dimension mappings" (length new-mappings))

    ;; Add to WordNet mappings
    (dolist (mapping new-mappings)
      (let ((word (car mapping))
            (dim (cdr mapping)))
        (unless (assoc word meta-log-wordnet--dimension-mappings)
          (push (cons word dim) meta-log-wordnet--dimension-mappings))))

    new-mappings))

;;; Enhancement 2: Discover Missing Modules

(defun meta-log-kg-discover-missing-modules ()
  "Analyze knowledge graph to find capabilities meta-log should have.
Returns list of suggested modules."
  (interactive)

  (let ((suggested-modules '())
        (common-topics (make-hash-table :test 'equal)))

    (message "Analyzing knowledge graph for missing capabilities...")

    ;; Find frequently occurring topics
    (maphash (lambda (id node)
               (let ((keywords (plist-get (meta-log-kg-node-properties node)
                                         :keywords)))
                 (dolist (word keywords)
                   (let ((word-lower (downcase word)))
                     (puthash word-lower
                             (1+ (or (gethash word-lower common-topics) 0))
                             common-topics)))))
             meta-log-kg--document-index)

    ;; Check against existing meta-log modules
    (let ((existing-modules '("prolog" "datalog" "r5rs" "wordnet" "template"
                             "federation" "crypto" "mqtt" "webrtc" "identity"
                             "protocol" "server" "collective" "verifiable"
                             "canvas" "geometric" "metacircular" "knowledge-graph")))

      (maphash (lambda (topic count)
                 (when (and (>= count 20)  ; Appears 20+ times
                           (not (member topic existing-modules)))

                   ;; Determine what kind of module this suggests
                   (let ((module-type
                          (cond
                           ((string-match-p "api\\|rest\\|http\\|endpoint" topic)
                            'network-api)
                           ((string-match-p "storage\\|database\\|persist" topic)
                            'storage-backend)
                           ((string-match-p "agent\\|llm\\|openai\\|claude" topic)
                            'ai-integration)
                           ((string-match-p "graph\\|topology\\|network" topic)
                            'graph-algorithms)
                           ((string-match-p "test\\|spec\\|assert" topic)
                            'testing-framework)
                           ((string-match-p "security\\|auth\\|permission" topic)
                            'security-layer)
                           ((string-match-p "ui\\|interface\\|visual" topic)
                            'user-interface)
                           (t 'general))))

                     (push (list :topic topic
                               :frequency count
                               :suggested-module (format "meta-log-%s" topic)
                               :type module-type)
                           suggested-modules))))
               common-topics))

    ;; Sort by frequency
    (setq suggested-modules
          (sort suggested-modules
                (lambda (a b)
                  (> (plist-get a :frequency)
                     (plist-get b :frequency)))))

    (message "Found %d suggested new modules" (length suggested-modules))
    suggested-modules))

;;; Enhancement 3: Generate Module from Pattern

(defun meta-log-kg-generate-module (topic)
  "Generate a new meta-log module for TOPIC based on knowledge graph.
Returns generated module code."
  (interactive "sTopic to generate module for: ")

  (message "Generating module for: %s" topic)

  ;; Find relevant documents
  (let* ((relevant-docs (meta-log-kg-query topic))
         (concepts '())
         (implementations '()))

    ;; Extract concepts and implementations
    (dolist (doc relevant-docs)
      (let ((label (meta-log-kg-node-label doc))
            (source (meta-log-kg-node-source-file doc)))
        (push label concepts)

        ;; Check if this is code
        (when (or (string-suffix-p ".el" source)
                  (string-suffix-p ".js" source)
                  (string-suffix-p ".ts" source))
          (push source implementations))))

    ;; Generate module template
    (let ((module-name (format "meta-log-%s" (downcase topic)))
          (module-code
           (format ";;; %s.el --- %s integration for meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Generated from Knowledge Graph
;; Version: 1.0.0

;;; Commentary:

;; This module was automatically generated by analyzing
;; %d documents in the knowledge graph about '%s'.
;;
;; Discovered concepts:
%s
;;
;; Based on implementations found in:
%s

;;; Code:

(require 'cl-lib)
(require 'meta-log-core)

(defvar meta-log-%s--initialized nil
  \"Whether %s module is initialized.\")

(defun meta-log-%s-initialize ()
  \"Initialize %s module.\"
  (interactive)
  (unless meta-log-%s--initialized
    (message \"Initializing %s module...\")
    ;; TODO: Add initialization based on discovered patterns
    (setq meta-log-%s--initialized t)
    (message \"✓ %s module initialized\")))

;; TODO: Add functions discovered from knowledge graph analysis
;; Relevant concepts found:
%s

(provide '%s)

;;; %s.el ends here
"
                   module-name
                   topic
                   (length relevant-docs)
                   topic
                   (mapconcat (lambda (c) (format ";;   - %s" c))
                             (cl-subseq concepts 0 (min 10 (length concepts)))
                             "\n")
                   (mapconcat (lambda (i) (format ";;   - %s" i))
                             (cl-subseq implementations 0 (min 5 (length implementations)))
                             "\n")
                   (downcase topic)
                   topic
                   (downcase topic)
                   topic
                   (downcase topic)
                   topic
                   (downcase topic)
                   topic
                   (mapconcat (lambda (c) (format ";; - %s" c))
                             (cl-subseq concepts 0 (min 20 (length concepts)))
                             "\n")
                   module-name
                   module-name)))

      (message "Generated module: %s" module-name)
      module-code)))

;;; Enhancement 4: Improve Template Discovery

(defun meta-log-kg-enhance-template-discovery ()
  "Use knowledge graph to improve template discovery.
Adds templates based on common patterns in documents."
  (interactive)

  (let ((new-templates '()))

    (message "Enhancing template discovery from knowledge graph...")

    ;; Find document clusters by semantic field
    (let ((field-clusters (make-hash-table :test 'equal)))

      (maphash (lambda (id node)
                 (let ((field (meta-log-kg-node-semantic-field node)))
                   (when field
                     (push node (gethash field field-clusters)))))
               meta-log-kg--document-index)

      ;; For each cluster, extract common patterns
      (maphash (lambda (field nodes)
                 (when (>= (length nodes) 5)
                   ;; Cluster has enough documents to form a template
                   (let* ((keywords-all (apply #'append
                                              (mapcar (lambda (n)
                                                       (plist-get (meta-log-kg-node-properties n)
                                                                 :keywords))
                                                     nodes)))
                          (keyword-freq (make-hash-table :test 'equal)))

                     ;; Count keyword frequencies
                     (dolist (kw keywords-all)
                       (puthash kw (1+ (or (gethash kw keyword-freq) 0))
                               keyword-freq))

                     ;; Find common keywords (appear in 30%+ of docs)
                     (let ((common-keywords '())
                           (threshold (* 0.3 (length nodes))))
                       (maphash (lambda (kw count)
                                  (when (>= count threshold)
                                    (push kw common-keywords)))
                               keyword-freq)

                       ;; Create template
                       (when common-keywords
                         (push (list :field field
                                   :keywords common-keywords
                                   :example-docs (length nodes))
                               new-templates))))))
               field-clusters))

    (message "Generated %d new template patterns" (length new-templates))
    new-templates))

;;; Enhancement 5: Self-Improvement Report

(defun meta-log-kg-generate-improvement-report ()
  "Generate comprehensive improvement report based on knowledge graph analysis.
Returns formatted report string."
  (interactive)

  (message "\nGenerating meta-log improvement report from knowledge graph...\n")

  (let ((patterns (meta-log-kg-discover-patterns))
        (new-mappings (meta-log-kg-learn-wordnet-mappings))
        (missing-modules (meta-log-kg-discover-missing-modules))
        (new-templates (meta-log-kg-enhance-template-discovery))
        (report ""))

    ;; Build report
    (setq report
          (concat
           "╔══════════════════════════════════════════════════════════╗\n"
           "║  META-LOG KNOWLEDGE GRAPH ENHANCEMENT REPORT            ║\n"
           "╚══════════════════════════════════════════════════════════╝\n\n"

           "SUMMARY\n"
           "=======\n"
           (format "Analyzed: %d documents, %d concepts\n"
                   (hash-table-count meta-log-kg--document-index)
                   (hash-table-count meta-log-kg--concept-index))
           (format "Discovered: %d patterns, %d new mappings, %d suggested modules\n\n"
                   (length patterns)
                   (length new-mappings)
                   (length missing-modules))

           "1. NEW WORDNET MAPPINGS LEARNED\n"
           "================================\n"
           (format "Learned %d new word-to-dimension mappings from your docs:\n\n"
                   (length new-mappings))
           (mapconcat (lambda (mapping)
                       (format "  • \"%s\" → %s dimension"
                               (car mapping) (cdr mapping)))
                     (cl-subseq new-mappings 0 (min 20 (length new-mappings)))
                     "\n")
           (if (> (length new-mappings) 20)
               (format "\n  ... and %d more\n" (- (length new-mappings) 20))
             "")
           "\n\n"

           "2. SUGGESTED NEW MODULES\n"
           "========================\n"
           (format "Found %d topics that could become meta-log modules:\n\n"
                   (length missing-modules))
           (mapconcat (lambda (mod)
                       (format "  • %s\n    Topic: %s\n    Frequency: %d documents\n    Type: %s"
                               (plist-get mod :suggested-module)
                               (plist-get mod :topic)
                               (plist-get mod :frequency)
                               (plist-get mod :type)))
                     (cl-subseq missing-modules 0 (min 10 (length missing-modules)))
                     "\n\n")
           (if (> (length missing-modules) 10)
               (format "\n\n  ... and %d more suggestions\n" (- (length missing-modules) 10))
             "")
           "\n\n"

           "3. COMMON PATTERNS DISCOVERED\n"
           "=============================\n"
           (format "Found %d recurring patterns across documents:\n\n"
                   (min 20 (length patterns)))
           (mapconcat (lambda (pattern)
                       (format "  • \"%s\" (appears %d times)"
                               (plist-get pattern :pattern)
                               (plist-get pattern :frequency)))
                     (cl-subseq patterns 0 (min 20 (length patterns)))
                     "\n")
           "\n\n"

           "4. NEW TEMPLATE PATTERNS\n"
           "========================\n"
           (format "Discovered %d template patterns from document clusters:\n\n"
                   (length new-templates))
           (mapconcat (lambda (tmpl)
                       (format "  • Field: %s\n    Keywords: %s\n    Based on: %d documents"
                               (plist-get tmpl :field)
                               (mapconcat 'identity
                                        (cl-subseq (plist-get tmpl :keywords)
                                                  0 (min 5 (length (plist-get tmpl :keywords))))
                                        ", ")
                               (plist-get tmpl :example-docs)))
                     (cl-subseq new-templates 0 (min 5 (length new-templates)))
                     "\n\n")
           "\n\n"

           "5. ACTIONABLE IMPROVEMENTS\n"
           "==========================\n\n"
           (format "✓ WordNet now knows %d more project-specific terms\n"
                   (length new-mappings))
           (format "✓ Can generate %d new modules from discovered patterns\n"
                   (length missing-modules))
           (format "✓ Template discovery enhanced with %d new patterns\n"
                   (length new-templates))
           "\n"
           "Next Steps:\n"
           "  1. Generate suggested modules: (meta-log-kg-generate-module \"topic\")\n"
           "  2. Export enhanced WordNet: Auto-applied to current session\n"
           "  3. Templates ready for use: (meta-log-discover-template \"...\")\n"
           "\n"
           "╚══════════════════════════════════════════════════════════╝\n"))

    (princ report)
    report))

(provide 'meta-log-kg-learning)

;;; meta-log-kg-learning.el ends here
