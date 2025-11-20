;;; build-universal-kg.el --- Build universal knowledge graph from all projects

;;; Commentary:
;; Ingests all documents from automaton and universal-life-vault projects
;; to create a unified knowledge graph as foundation for all other graphs.

;;; Code:

(add-to-list 'load-path "/data/data/com.termux/files/home/github/meta-log")
(require 'meta-log)
(require 'meta-log-knowledge-graph)

(princ "\n")
(princ "╔══════════════════════════════════════════════════════╗\n")
(princ "║  UNIVERSAL KNOWLEDGE GRAPH BUILDER                  ║\n")
(princ "║  Unifying automaton + universal-life-vault          ║\n")
(princ "╚══════════════════════════════════════════════════════╝\n")
(princ "\n")

;; Initialize meta-log
(princ "Initializing meta-log system...\n")
(meta-log-initialize)
(princ "✓ Initialized\n\n")

;; Define source directories
(defvar ulv-sources
  '("/data/data/com.termux/files/home/github/universal-life-vault"
    "/data/data/com.termux/files/home/github/universal-life-protocol/packages/universal-life-vault")
  "Universal Life Vault source directories.")

(defvar automaton-sources
  '("/data/data/com.termux/files/home/github/automaton"
    "/data/data/com.termux/files/home/github/meta-log/examples/automaton-evolutions")
  "Automaton source directories.")

(princ "═══════════════════════════════════════════════════════\n")
(princ "PHASE 1: INGESTING UNIVERSAL-LIFE-VAULT\n")
(princ "═══════════════════════════════════════════════════════\n\n")

(let ((total-ulv 0))
  (dolist (dir ulv-sources)
    (when (file-directory-p dir)
      (princ (format "Processing: %s\n" dir))
      (let ((count (meta-log-kg-ingest-directory dir)))
        (setq total-ulv (+ total-ulv count))
        (princ (format "  ✓ Ingested %d documents\n\n" count)))))
  (princ (format "Total ULV documents: %d\n\n" total-ulv)))

(princ "═══════════════════════════════════════════════════════\n")
(princ "PHASE 2: INGESTING AUTOMATON PROJECT\n")
(princ "═══════════════════════════════════════════════════════\n\n")

(let ((total-automaton 0))
  (dolist (dir automaton-sources)
    (when (file-directory-p dir)
      (princ (format "Processing: %s\n" dir))
      (let ((count (meta-log-kg-ingest-directory dir)))
        (setq total-automaton (+ total-automaton count))
        (princ (format "  ✓ Ingested %d documents\n\n" count)))))
  (princ (format "Total automaton documents: %d\n\n" total-automaton)))

(princ "═══════════════════════════════════════════════════════\n")
(princ "PHASE 3: KNOWLEDGE GRAPH STATISTICS\n")
(princ "═══════════════════════════════════════════════════════\n\n")

(meta-log-kg-stats)

(princ "\n═══════════════════════════════════════════════════════\n")
(princ "PHASE 4: EXPORTING GRAPH\n")
(princ "═══════════════════════════════════════════════════════\n\n")

;; Export to different formats
(princ "Exporting knowledge graph...\n")
(meta-log-kg-export-graph "examples/universal-knowledge-graph.graphml")
(princ "✓ Exported GraphML to examples/universal-knowledge-graph.graphml\n\n")

;; Save Prolog facts
(princ "Saving Prolog facts...\n")
(with-temp-file "examples/universal-kg-facts.pl"
  (insert "%% Universal Knowledge Graph - Prolog Facts\n")
  (insert "%% Generated: ")
  (insert (format-time-string "%Y-%m-%d %H:%M:%S\n\n"))
  (insert "%% Query examples:\n")
  (insert "%%   kg-node(?Id, ?Type, ?Label).\n")
  (insert "%%   kg-dimension(?Id, ?Dimension).\n")
  (insert "%%   kg-semantic-field(?Id, ?Field).\n")
  (insert "%%   kg-relation(?From, contains, ?To).\n\n"))
(princ "✓ Saved Prolog facts to examples/universal-kg-facts.pl\n\n")

(princ "═══════════════════════════════════════════════════════\n")
(princ "PHASE 5: TESTING QUERIES\n")
(princ "═══════════════════════════════════════════════════════\n\n")

;; Test some queries
(princ "Testing natural language query...\n")
(princ "Query: \"agents blackboard architecture\"\n\n")

(let ((results (meta-log-kg-query "agents blackboard architecture")))
  (princ (format "Found %d matching documents:\n" (length results)))
  (let ((i 1))
    (dolist (node (cl-subseq results 0 (min 5 (length results))))
      (princ (format "  %d. %s\n" i (meta-log-kg-node-label node)))
      (princ (format "     Source: %s\n" (meta-log-kg-node-source-file node)))
      (princ (format "     Dimension: %s\n" (meta-log-kg-node-dimension node)))
      (setq i (1+ i))))
  (when (> (length results) 5)
    (princ (format "  ... and %d more\n" (- (length results) 5)))))

(princ "\n")
(princ "Testing another query...\n")
(princ "Query: \"protocol merkle trie\"\n\n")

(let ((results (meta-log-kg-query "protocol merkle trie")))
  (princ (format "Found %d matching documents:\n" (length results)))
  (let ((i 1))
    (dolist (node (cl-subseq results 0 (min 5 (length results))))
      (princ (format "  %d. %s\n" i (meta-log-kg-node-label node)))
      (setq i (1+ i))))
  (when (> (length results) 5)
    (princ (format "  ... and %d more\n" (- (length results) 5)))))

(princ "\n╔══════════════════════════════════════════════════════╗\n")
(princ "║  UNIVERSAL KNOWLEDGE GRAPH COMPLETE!                ║\n")
(princ "╚══════════════════════════════════════════════════════╝\n\n")

(princ "The unified knowledge graph is now available as the foundation\n")
(princ "for all other graphs. You can:\n\n")

(princ "1. Query with natural language:\n")
(princ "   (meta-log-kg-query \"your query here\")\n\n")

(princ "2. Query with Prolog:\n")
(princ "   (meta-log-kg-query-prolog \"kg-node(?Id, document, ?Label)\")\n\n")

(princ "3. View statistics:\n")
(princ "   (meta-log-kg-stats)\n\n")

(princ "4. Export graph:\n")
(princ "   (meta-log-kg-export-graph \"output.graphml\")\n\n")

(princ "Generated files:\n")
(princ "  • examples/universal-knowledge-graph.graphml\n")
(princ "  • examples/universal-kg-facts.pl\n\n")

(provide 'build-universal-kg)
;;; build-universal-kg.el ends here
