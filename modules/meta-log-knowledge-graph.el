;;; meta-log-knowledge-graph.el --- Universal knowledge graph builder

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Build a unified knowledge graph from multiple document sources.
;; Ingests markdown, org files, and code to create a foundational
;; knowledge base for all other graphs.

;;; Code:

(require 'cl-lib)
(require 'meta-log-core)
(require 'meta-log-prolog)
(require 'meta-log-datalog)
(require 'meta-log-wordnet)
(require 'org)

;;; Knowledge Graph Structure

(defvar meta-log-kg--graph nil
  "The universal knowledge graph.")

(defvar meta-log-kg--document-index (make-hash-table :test 'equal)
  "Index of all ingested documents.")

(defvar meta-log-kg--concept-index (make-hash-table :test 'equal)
  "Index of all extracted concepts.")

(cl-defstruct meta-log-kg-node
  "Knowledge graph node."
  id
  type                 ; document, concept, entity, relation
  label
  properties
  dimension
  semantic-field
  source-file
  related-nodes)

(cl-defstruct meta-log-kg-edge
  "Knowledge graph edge."
  id
  from-node
  to-node
  relation-type        ; contains, references, implements, extends
  weight
  properties)

;;; Document Ingestion

(defun meta-log-kg-ingest-directory (directory)
  "Ingest all documents from DIRECTORY into knowledge graph.
Returns number of documents processed."
  (interactive "DDirectory to ingest: ")

  (let ((files '())
        (count 0))

    (message "Scanning directory: %s" directory)

    ;; Find all markdown and org files
    (setq files (append
                 (directory-files-recursively directory "\\.md$")
                 (directory-files-recursively directory "\\.org$")))

    (message "Found %d files to process" (length files))

    ;; Process each file
    (dolist (file files)
      (when (meta-log-kg-ingest-file file)
        (setq count (1+ count)))
      (when (zerop (mod count 10))
        (message "Processed %d/%d files..." count (length files))))

    (message "✓ Ingested %d documents" count)
    count))

(defun meta-log-kg-ingest-file (file-path)
  "Ingest a single file into the knowledge graph.
FILE-PATH is the path to the file.
Returns t on success."
  (when (file-exists-p file-path)
    (let* ((content (with-temp-buffer
                      (insert-file-contents file-path)
                      (buffer-string)))
           (doc-id (secure-hash 'sha256 file-path))
           (doc-node (meta-log-kg-extract-document-info file-path content)))

      ;; Store document
      (puthash doc-id doc-node meta-log-kg--document-index)

      ;; Extract concepts
      (meta-log-kg-extract-concepts doc-node content)

      ;; Add to Prolog database
      (meta-log-kg-add-to-prolog doc-node)

      ;; Add to Datalog database
      (meta-log-kg-add-to-datalog doc-node)

      t)))

(defun meta-log-kg-extract-document-info (file-path content)
  "Extract document information from FILE-PATH and CONTENT.
Returns a kg-node structure."
  (let* ((filename (file-name-nondirectory file-path))
         (extension (file-name-extension file-path))
         (title (meta-log-kg-extract-title content extension))
         (keywords (meta-log-kg-extract-keywords content))
         (dimension (meta-log-kg-infer-dimension keywords content))
         (semantic-field (meta-log-kg-infer-semantic-field keywords)))

    (make-meta-log-kg-node
     :id (secure-hash 'sha256 file-path)
     :type 'document
     :label (or title filename)
     :properties (list :filename filename
                      :path file-path
                      :extension extension
                      :size (length content)
                      :keywords keywords)
     :dimension dimension
     :semantic-field semantic-field
     :source-file file-path
     :related-nodes '())))

(defun meta-log-kg-extract-title (content extension)
  "Extract title from CONTENT based on EXTENSION.
Returns title string or nil."
  (cond
   ;; Markdown
   ((string= extension "md")
    (when (string-match "^#\\s-+\\(.+\\)$" content)
      (match-string 1 content)))

   ;; Org Mode
   ((string= extension "org")
    (when (string-match "^#\\+TITLE:\\s-+\\(.+\\)$" content)
      (match-string 1 content)))

   (t nil)))

(defun meta-log-kg-extract-keywords (content)
  "Extract keywords from CONTENT using WordNet.
Returns list of keyword strings."
  (require 'meta-log-wordnet)
  (let* ((text (substring content 0 (min 1000 (length content))))
         (keywords (meta-log-wordnet-extract-keywords text)))
    (mapcar (lambda (kw) (plist-get kw :word)) keywords)))

(defun meta-log-kg-infer-dimension (keywords content)
  "Infer dimension from KEYWORDS and CONTENT.
Returns dimension string (0D-7D)."
  (require 'meta-log-wordnet)
  (let ((text (substring content 0 (min 2000 (length content)))))
    (or (meta-log-wordnet-map-to-dimension text)
        "2D"))) ; Default to 2D (structure)

(defun meta-log-kg-infer-semantic-field (keywords)
  "Infer semantic field from KEYWORDS.
Returns semantic field string."
  (require 'meta-log-wordnet)
  (let ((fields (make-hash-table :test 'equal)))
    (dolist (keyword keywords)
      (let ((field (meta-log-wordnet-map-word-to-semantic-field keyword)))
        (when field
          (puthash field (1+ (or (gethash field fields) 0)) fields))))

    ;; Return most common field
    (let ((best-field nil)
          (best-count 0))
      (maphash (lambda (field count)
                 (when (> count best-count)
                   (setq best-count count)
                   (setq best-field field)))
               fields)
      best-field)))

;;; Concept Extraction

(defun meta-log-kg-extract-concepts (doc-node content)
  "Extract concepts from CONTENT and link to DOC-NODE.
Adds concepts to the knowledge graph."
  (let ((concepts '()))

    ;; Extract headings as concepts
    (dolist (heading (meta-log-kg-extract-headings content))
      (let* ((concept-id (secure-hash 'sha256 (concat (meta-log-kg-node-id doc-node) heading)))
             (concept-node (make-meta-log-kg-node
                            :id concept-id
                            :type 'concept
                            :label heading
                            :properties (list :source-doc (meta-log-kg-node-id doc-node))
                            :dimension (meta-log-kg-node-dimension doc-node)
                            :semantic-field (meta-log-kg-node-semantic-field doc-node)
                            :source-file (meta-log-kg-node-source-file doc-node)
                            :related-nodes (list (meta-log-kg-node-id doc-node)))))

        (puthash concept-id concept-node meta-log-kg--concept-index)
        (push concept-id concepts)))

    ;; Extract mathematical concepts if available
    (when (fboundp 'meta-log-kg-extract-mathematical-concepts)
      (let ((math-concepts (meta-log-kg-extract-mathematical-concepts content)))
        (dolist (math-concept math-concepts)
          (let* ((concept-id (secure-hash 'sha256 (format "%s-%s"
                                                          (meta-log-kg-node-id doc-node)
                                                          (plist-get math-concept :name))))
                 (concept-node (make-meta-log-kg-node
                                :id concept-id
                                :type 'mathematical-concept
                                :label (plist-get math-concept :name)
                                :properties math-concept
                                :dimension (plist-get math-concept :dimension)
                                :semantic-field 'mathematics
                                :source-file (meta-log-kg-node-source-file doc-node)
                                :related-nodes (list (meta-log-kg-node-id doc-node)))))
            (puthash concept-id concept-node meta-log-kg--concept-index)
            (push concept-id concepts)))))

    ;; Link concepts to document
    (setf (meta-log-kg-node-related-nodes doc-node) concepts)

    concepts))

(defun meta-log-kg-extract-headings (content)
  "Extract headings from markdown/org CONTENT.
Returns list of heading strings."
  (let ((headings '()))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Match markdown and org headings
      (while (re-search-forward "^\\(#+\\|\\*+\\)\\s-+\\(.+\\)$" nil t)
        (push (match-string 2) headings)))
    (nreverse headings)))

;;; Add to Logic Engines

(defun meta-log-kg-add-to-prolog (node)
  "Add NODE to Prolog database as facts.
Creates document/concept facts."
  (require 'meta-log-prolog)

  (let ((id (meta-log-kg-node-id node))
        (type (meta-log-kg-node-type node))
        (label (meta-log-kg-node-label node))
        (dimension (meta-log-kg-node-dimension node))
        (semantic-field (meta-log-kg-node-semantic-field node))
        (source (meta-log-kg-node-source-file node)))

    ;; Add base facts
    (meta-log-prolog-add-fact 'kg-node id (symbol-name type) label)
    (when dimension
      (meta-log-prolog-add-fact 'kg-dimension id dimension))
    (when semantic-field
      (meta-log-prolog-add-fact 'kg-semantic-field id semantic-field))
    (when source
      (meta-log-prolog-add-fact 'kg-source id source))

    ;; Add relations
    (dolist (related-id (meta-log-kg-node-related-nodes node))
      (meta-log-prolog-add-fact 'kg-relation id "contains" related-id))))

(defun meta-log-kg-add-to-datalog (node)
  "Add NODE to Datalog database as facts."
  (require 'meta-log-datalog)

  (let ((id (meta-log-kg-node-id node))
        (type (meta-log-kg-node-type node))
        (label (meta-log-kg-node-label node)))

    (meta-log-datalog-add-fact 'kg-node id (symbol-name type) label)))

;;; Query Interface

(defun meta-log-kg-query (query-string)
  "Query the knowledge graph using natural language QUERY-STRING.
Returns list of matching nodes."
  (interactive "sQuery: ")

  (require 'meta-log-wordnet)

  (let* ((keywords (meta-log-wordnet-extract-keywords query-string))
         (keyword-words (mapcar (lambda (k) (plist-get k :word)) keywords))
         (matches '()))

    (message "Searching for: %S" keyword-words)

    ;; Search documents
    (maphash (lambda (id node)
               (let ((doc-keywords (plist-get (meta-log-kg-node-properties node) :keywords))
                     (label (meta-log-kg-node-label node)))
                 (when (or (cl-intersection keyword-words doc-keywords :test 'string=)
                          (cl-some (lambda (kw) (string-match-p kw label)) keyword-words))
                   (push node matches))))
             meta-log-kg--document-index)

    ;; Search concepts
    (maphash (lambda (id node)
               (let ((label (meta-log-kg-node-label node)))
                 (when (cl-some (lambda (kw) (string-match-p kw (downcase label)))
                               keyword-words)
                   (push node matches))))
             meta-log-kg--concept-index)

    (message "Found %d matches" (length matches))
    matches))

(defun meta-log-kg-query-prolog (prolog-query)
  "Query knowledge graph using Prolog PROLOG-QUERY.
Returns Prolog query results."
  (interactive "sProlog query: ")
  (require 'meta-log-prolog)
  (meta-log-prolog-query prolog-query))

;;; Visualization and Export

(defun meta-log-kg-export-graph (output-file)
  "Export knowledge graph to OUTPUT-FILE in GraphML format."
  (interactive "FOutput file: ")

  (with-temp-file output-file
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (insert "<graphml>\n")
    (insert "  <graph id=\"meta-log-kg\" edgedefault=\"directed\">\n")

    ;; Export nodes
    (maphash (lambda (id node)
               (insert (format "    <node id=\"%s\">\n" id))
               (insert (format "      <data key=\"label\">%s</data>\n"
                             (meta-log-kg-node-label node)))
               (insert (format "      <data key=\"type\">%s</data>\n"
                             (meta-log-kg-node-type node)))
               (when (meta-log-kg-node-dimension node)
                 (insert (format "      <data key=\"dimension\">%s</data>\n"
                               (meta-log-kg-node-dimension node))))
               (insert "    </node>\n"))
             meta-log-kg--document-index)

    (maphash (lambda (id node)
               (insert (format "    <node id=\"%s\">\n" id))
               (insert (format "      <data key=\"label\">%s</data>\n"
                             (meta-log-kg-node-label node)))
               (insert (format "      <data key=\"type\">%s</data>\n"
                             (meta-log-kg-node-type node)))
               (insert "    </node>\n"))
             meta-log-kg--concept-index)

    ;; Export edges
    (let ((edge-id 0))
      (maphash (lambda (id node)
                 (dolist (related-id (meta-log-kg-node-related-nodes node))
                   (insert (format "    <edge id=\"e%d\" source=\"%s\" target=\"%s\"/>\n"
                                 edge-id id related-id))
                   (setq edge-id (1+ edge-id))))
               meta-log-kg--document-index))

    (insert "  </graph>\n")
    (insert "</graphml>\n"))

  (message "✓ Exported knowledge graph to %s" output-file))

(defun meta-log-kg-stats ()
  "Display knowledge graph statistics."
  (interactive)

  (let ((doc-count (hash-table-count meta-log-kg--document-index))
        (concept-count (hash-table-count meta-log-kg--concept-index))
        (dimensions (make-hash-table :test 'equal))
        (fields (make-hash-table :test 'equal)))

    ;; Count dimensions and fields
    (maphash (lambda (id node)
               (let ((dim (meta-log-kg-node-dimension node))
                     (field (meta-log-kg-node-semantic-field node)))
                 (when dim
                   (puthash dim (1+ (or (gethash dim dimensions) 0)) dimensions))
                 (when field
                   (puthash field (1+ (or (gethash field fields) 0)) fields))))
             meta-log-kg--document-index)

    (message "\n=== Knowledge Graph Statistics ===")
    (message "Documents: %d" doc-count)
    (message "Concepts: %d" concept-count)
    (message "Total nodes: %d" (+ doc-count concept-count))
    (message "\nBy Dimension:")
    (maphash (lambda (dim count)
               (message "  %s: %d" dim count))
             dimensions)
    (message "\nBy Semantic Field:")
    (maphash (lambda (field count)
               (message "  %s: %d" field count))
             fields)
    (message "================================\n")))

(provide 'meta-log-knowledge-graph)

;;; meta-log-knowledge-graph.el ends here
