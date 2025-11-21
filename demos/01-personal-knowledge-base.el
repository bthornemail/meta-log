;;; 01-personal-knowledge-base.el --- Demo: Building a Personal Knowledge Base

;; This demo shows how to use meta-log to create and query
;; a personal knowledge base from your notes.

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'meta-log)
(require 'meta-log-ingest)
(require 'meta-log-knowledge-graph)

(defun demo-personal-kb-setup ()
  "Set up demo knowledge base with sample notes."
  (interactive)
  (message "🚀 Demo: Personal Knowledge Base")
  (message "")
  (message "Setting up your personal knowledge base...")

  ;; Initialize meta-log
  (meta-log-initialize)
  (message "✓ meta-log initialized")

  ;; Create sample notes directory
  (let ((notes-dir (expand-file-name "~/demo-notes")))
    (unless (file-exists-p notes-dir)
      (make-directory notes-dir))

    ;; Create sample note files
    (with-temp-file (expand-file-name "programming.org" notes-dir)
      (insert "#+TITLE: Programming Notes\n\n")
      (insert "* Functional Programming\n")
      (insert "Functional programming emphasizes immutability and pure functions.\n\n")
      (insert "** Lisp Family\n")
      (insert "- Scheme: Minimalist dialect\n")
      (insert "- Common Lisp: Feature-rich, practical\n")
      (insert "- Clojure: Modern, runs on JVM\n\n")
      (insert "* Logic Programming\n")
      (insert "** Prolog\n")
      (insert "Prolog uses declarative rules and facts for computation.\n"))

    (with-temp-file (expand-file-name "machine-learning.org" notes-dir)
      (insert "#+TITLE: Machine Learning Notes\n\n")
      (insert "* Neural Networks\n")
      (insert "Neural networks are computational models inspired by biological brains.\n\n")
      (insert "** Deep Learning\n")
      (insert "Deep learning uses multiple layers to learn hierarchical representations.\n\n")
      (insert "* Natural Language Processing\n")
      (insert "NLP enables computers to understand and generate human language.\n"))

    (with-temp-file (expand-file-name "books.org" notes-dir)
      (insert "#+TITLE: Book Notes\n\n")
      (insert "* SICP - Structure and Interpretation of Computer Programs\n")
      (insert "Classic CS textbook using Scheme.\n")
      (insert "Teaches fundamental programming concepts.\n\n")
      (insert "* The Art of Prolog\n")
      (insert "Comprehensive guide to logic programming.\n"))

    (message "✓ Created sample notes in %s" notes-dir)

    ;; Ingest the notes directory
    (message "")
    (message "Ingesting notes into knowledge graph...")
    (meta-log-ingest-directory notes-dir)
    (message "✓ Notes ingested")

    notes-dir))

(defun demo-personal-kb-query ()
  "Demonstrate querying the knowledge base."
  (interactive)
  (message "")
  (message "📚 Querying Your Knowledge Base")
  (message "")

  ;; Search for programming-related topics
  (message "Q: What do I know about functional programming?")
  (let ((results (meta-log-kg-search "functional programming")))
    (message "A: Found %d related entries" (length results))
    (dolist (result results)
      (message "  - %s" result)))
  (message "")

  ;; Search for books
  (message "Q: What books have I read about programming?")
  (let ((results (meta-log-kg-search "book programming")))
    (message "A: Found %d related entries" (length results))
    (dolist (result results)
      (message "  - %s" result)))
  (message "")

  ;; Natural language query
  (message "Q: Tell me about Lisp")
  (let ((response (meta-log-ask "What do I know about Lisp?")))
    (message "A: %s" response))
  (message ""))

(defun demo-personal-kb-insights ()
  "Show insights and connections in the knowledge base."
  (interactive)
  (message "")
  (message "🔍 Knowledge Graph Insights")
  (message "")

  ;; Show related concepts
  (message "Finding connections between concepts...")
  (let ((connections (meta-log-kg-find-related "Prolog" 2)))
    (message "Concepts related to Prolog:")
    (dolist (conn connections)
      (message "  - %s" conn)))
  (message "")

  ;; Show knowledge statistics
  (message "Knowledge Base Statistics:")
  (message "  Total nodes: %d" (meta-log-kg-node-count))
  (message "  Total edges: %d" (meta-log-kg-edge-count))
  (message "  Topics: %d" (length (meta-log-kg-get-topics)))
  (message ""))

(defun demo-personal-kb-full ()
  "Run the complete personal knowledge base demo."
  (interactive)
  (message "")
  (message "════════════════════════════════════════")
  (message "  Personal Knowledge Base Demo")
  (message "════════════════════════════════════════")
  (message "")

  (let ((notes-dir (demo-personal-kb-setup)))
    (message "")
    (message "═══════════════════════════════════════")
    (demo-personal-kb-query)
    (message "═══════════════════════════════════════")
    (demo-personal-kb-insights)
    (message "")
    (message "✨ Demo complete!")
    (message "")
    (message "Next steps:")
    (message "  1. Try: M-x meta-log-ingest-directory to add your own notes")
    (message "  2. Try: M-x meta-log-ask to query your knowledge")
    (message "  3. Try: M-x meta-log-dashboard to see the visual interface")
    (message "")
    (message "Your demo notes are in: %s" notes-dir)))

;; Run demo
(when (called-interactively-p 'any)
  (demo-personal-kb-full))

(provide '01-personal-knowledge-base)
;;; 01-personal-knowledge-base.el ends here
