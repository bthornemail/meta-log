;;; 02-research-assistant.el --- Demo: AI Research Assistant

;; This demo shows how to use meta-log as an AI-powered research assistant
;; that learns from your interactions and helps you explore topics.

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'meta-log)
(require 'meta-log-chat)
(require 'meta-log-llm)
(require 'meta-log-llm-learning)

(defun demo-research-setup ()
  "Set up the research assistant demo."
  (interactive)
  (message "🔬 Demo: AI Research Assistant")
  (message "")
  (message "Initializing research assistant...")

  ;; Initialize meta-log
  (meta-log-initialize)
  (message "✓ meta-log initialized")

  ;; Set up research context
  (message "✓ Research context ready")
  (message ""))

(defun demo-research-learning ()
  "Demonstrate the learning capabilities."
  (interactive)
  (message "📖 Learning and Adaptation")
  (message "")

  ;; Teach the system about your research area
  (message "Teaching the system about quantum computing...")
  (meta-log-llm-record-interaction
   "What is quantum superposition?"
   "Quantum superposition is a principle where quantum systems can exist in multiple states simultaneously until measured.")

  (meta-log-llm-record-interaction
   "What are qubits?"
   "Qubits are quantum bits that can represent 0, 1, or both simultaneously through superposition.")

  (meta-log-llm-record-interaction
   "What is quantum entanglement?"
   "Quantum entanglement is a phenomenon where quantum particles become correlated in ways that classical particles cannot.")

  (message "✓ Learned 3 concepts about quantum computing")
  (message "")

  ;; Now the system can use this knowledge
  (message "Testing learned knowledge...")
  (let ((response (meta-log-ask "Explain how qubits use superposition")))
    (message "Q: Explain how qubits use superposition")
    (message "A: %s" response))
  (message ""))

(defun demo-research-queries ()
  "Demonstrate research query capabilities."
  (interactive)
  (message "🔎 Research Queries")
  (message "")

  ;; Add research knowledge to the system
  (meta-log-prolog-assert "paper(p1, 'Neural Networks for NLP', 2020)")
  (meta-log-prolog-assert "paper(p2, 'Attention Mechanisms', 2019)")
  (meta-log-prolog-assert "paper(p3, 'Transformer Architecture', 2017)")
  (meta-log-prolog-assert "cites(p1, p2)")
  (meta-log-prolog-assert "cites(p2, p3)")
  (meta-log-prolog-assert "author(p1, 'Dr. Smith')")
  (meta-log-prolog-assert "author(p2, 'Dr. Johnson')")
  (meta-log-prolog-assert "topic(p1, nlp)")
  (meta-log-prolog-assert "topic(p2, attention)")

  (message "Added research paper data to knowledge base")
  (message "")

  ;; Query papers by topic
  (message "Q: What papers are about NLP?")
  (let ((results (meta-log-prolog-query "paper(ID, Title, Year), topic(ID, nlp)")))
    (message "A: Found papers:")
    (dolist (result results)
      (message "  - %s" result)))
  (message "")

  ;; Query citation relationships
  (message "Q: Which papers cite 'Transformer Architecture'?")
  (let ((results (meta-log-prolog-query "paper(P, Title, _), cites(P, p3)")))
    (message "A: Papers citing Transformer Architecture:")
    (dolist (result results)
      (message "  - %s" result)))
  (message ""))

(defun demo-research-chat ()
  "Demonstrate interactive research chat."
  (interactive)
  (message "💬 Interactive Research Chat")
  (message "")

  (message "Starting research conversation...")
  (message "")

  ;; Simulate a research conversation
  (let ((questions '("What are the main approaches to natural language processing?"
                     "How do transformer models work?"
                     "What are the limitations of current NLP systems?")))
    (dolist (question questions)
      (message "You: %s" question)
      (let ((response (meta-log-chat-send question)))
        (message "Assistant: %s" (or response "Let me research that for you..."))
        (message ""))))

  ;; Show conversation history
  (message "Conversation summary:")
  (let ((history (meta-log-chat-get-history)))
    (message "  Messages exchanged: %d" (length history)))
  (message ""))

(defun demo-research-synthesis ()
  "Demonstrate knowledge synthesis."
  (interactive)
  (message "🧩 Knowledge Synthesis")
  (message "")

  ;; Add diverse knowledge
  (meta-log-kg-add-node "Machine Learning" "topic")
  (meta-log-kg-add-node "Deep Learning" "topic")
  (meta-log-kg-add-node "Neural Networks" "technique")
  (meta-log-kg-add-edge "Deep Learning" "is-subset-of" "Machine Learning")
  (meta-log-kg-add-edge "Deep Learning" "uses" "Neural Networks")

  (message "Finding connections between concepts...")
  (let ((connections (meta-log-kg-find-path "Deep Learning" "Machine Learning")))
    (message "Path from 'Deep Learning' to 'Machine Learning':")
    (message "  %s" (mapconcat 'identity connections " → ")))
  (message "")

  ;; Generate research summary
  (message "Generating research summary...")
  (let ((summary (meta-log-ask "Summarize what we've learned about machine learning and NLP")))
    (message "Research Summary:")
    (message "%s" summary))
  (message ""))

(defun demo-research-full ()
  "Run the complete research assistant demo."
  (interactive)
  (message "")
  (message "════════════════════════════════════════")
  (message "  AI Research Assistant Demo")
  (message "════════════════════════════════════════")
  (message "")

  (demo-research-setup)
  (message "═══════════════════════════════════════")
  (demo-research-learning)
  (message "═══════════════════════════════════════")
  (demo-research-queries)
  (message "═══════════════════════════════════════")
  (demo-research-chat)
  (message "═══════════════════════════════════════")
  (demo-research-synthesis)

  (message "")
  (message "✨ Demo complete!")
  (message "")
  (message "Key features demonstrated:")
  (message "  ✓ Learning from interactions")
  (message "  ✓ Querying research knowledge")
  (message "  ✓ Interactive chat interface")
  (message "  ✓ Knowledge synthesis and connections")
  (message "")
  (message "Next steps:")
  (message "  1. Try: M-x meta-log-chat to start your own research conversation")
  (message "  2. Try: M-x meta-log-ask to query your research")
  (message "  3. Add your own papers and notes with M-x meta-log-ingest-directory"))

;; Run demo
(when (called-interactively-p 'any)
  (demo-research-full))

(provide '02-research-assistant)
;;; 02-research-assistant.el ends here
