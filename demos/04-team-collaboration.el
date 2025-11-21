;;; 04-team-collaboration.el --- Demo: Team Knowledge Sharing

;; This demo shows how to use meta-log for team collaboration,
;; federated knowledge sharing, and distributed learning.

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'meta-log)
(require 'meta-log-federation)
(require 'meta-log-identity)

(defun demo-collab-setup ()
  "Set up team collaboration demo."
  (interactive)
  (message "👥 Demo: Team Knowledge Sharing")
  (message "")
  (message "Setting up team collaboration environment...")

  ;; Initialize meta-log
  (meta-log-initialize)
  (message "✓ meta-log initialized")

  ;; Initialize federation
  (meta-log-federation-init)
  (message "✓ Federation initialized")

  ;; Create team identities
  (let ((alice (meta-log-identity-create-peer))
        (bob (meta-log-identity-create-peer))
        (charlie (meta-log-identity-create-peer)))
    (message "✓ Created team member identities")
    (message "  - Alice: %s" (meta-log-identity-get-peer-id alice))
    (message "  - Bob: %s" (meta-log-identity-get-peer-id bob))
    (message "  - Charlie: %s" (meta-log-identity-get-peer-id charlie))
    (message "")

    (list alice bob charlie)))

(defun demo-collab-knowledge-sharing ()
  "Demonstrate knowledge sharing between team members."
  (interactive)
  (message "📤 Knowledge Sharing")
  (message "")

  ;; Alice shares frontend knowledge
  (message "Alice shares frontend expertise...")
  (meta-log-kg-add-node "React Patterns" "knowledge")
  (meta-log-kg-add-node "Component Design" "knowledge")
  (meta-log-kg-add-edge "React Patterns" "includes" "Component Design")
  (meta-log-kg-add-tag "React Patterns" "frontend")
  (meta-log-kg-add-tag "React Patterns" "shared-by:alice")

  ;; Bob shares backend knowledge
  (message "Bob shares backend expertise...")
  (meta-log-kg-add-node "Database Optimization" "knowledge")
  (meta-log-kg-add-node "Query Performance" "knowledge")
  (meta-log-kg-add-edge "Database Optimization" "includes" "Query Performance")
  (meta-log-kg-add-tag "Database Optimization" "backend")
  (meta-log-kg-add-tag "Database Optimization" "shared-by:bob")

  ;; Charlie shares DevOps knowledge
  (message "Charlie shares DevOps expertise...")
  (meta-log-kg-add-node "Container Orchestration" "knowledge")
  (meta-log-kg-add-node "Kubernetes Patterns" "knowledge")
  (meta-log-kg-add-edge "Container Orchestration" "includes" "Kubernetes Patterns")
  (meta-log-kg-add-tag "Container Orchestration" "devops")
  (meta-log-kg-add-tag "Container Orchestration" "shared-by:charlie")

  (message "✓ Team knowledge shared")
  (message ""))

(defun demo-collab-discovery ()
  "Demonstrate discovering team expertise."
  (interactive)
  (message "🔍 Discovering Team Expertise")
  (message "")

  ;; Find experts by topic
  (message "Q: Who knows about frontend development?")
  (let ((experts (meta-log-kg-search "frontend")))
    (message "A: Frontend experts:")
    (dolist (expert experts)
      (message "  - %s" expert)))
  (message "")

  (message "Q: Who can help with database issues?")
  (let ((experts (meta-log-kg-search "backend database")))
    (message "A: Database experts:")
    (dolist (expert experts)
      (message "  - %s" expert)))
  (message "")

  ;; Find related knowledge across team
  (message "Q: What knowledge is related to performance?")
  (let ((topics (meta-log-kg-search "performance")))
    (message "A: Performance-related topics:")
    (dolist (topic topics)
      (message "  - %s" topic)))
  (message ""))

(defun demo-collab-federated-learning ()
  "Demonstrate federated learning across team."
  (interactive)
  (message "🧠 Federated Learning")
  (message "")

  ;; Team members contribute to shared understanding
  (message "Building shared team understanding...")

  ;; Alice's perspective on system architecture
  (meta-log-llm-record-interaction
   "What are microservices?"
   "Microservices are independently deployable services that communicate via APIs. (Alice's perspective: Focus on frontend integration)")

  ;; Bob's perspective
  (meta-log-llm-record-interaction
   "What are microservices?"
   "Microservices are isolated services with their own databases. (Bob's perspective: Focus on data isolation)")

  ;; Charlie's perspective
  (meta-log-llm-record-interaction
   "What are microservices?"
   "Microservices are containers orchestrated for scalability. (Charlie's perspective: Focus on deployment)")

  (message "✓ Multiple perspectives integrated")
  (message "")

  ;; Query combines all perspectives
  (message "Q: Explain microservices architecture")
  (let ((answer (meta-log-ask "What are microservices and how do they work?")))
    (message "A: Combined team knowledge:")
    (message "%s" (or answer "Microservices integrate multiple architectural concerns: frontend, backend, and deployment.")))
  (message ""))

(defun demo-collab-code-review ()
  "Demonstrate collaborative code review using meta-log."
  (interactive)
  (message "👁️ Collaborative Code Review")
  (message "")

  ;; Add code review data
  (meta-log-prolog-assert "pull_request(pr123, 'Add user authentication', alice)")
  (meta-log-prolog-assert "reviewer(pr123, bob)")
  (meta-log-prolog-assert "reviewer(pr123, charlie)")
  (meta-log-prolog-assert "comment(pr123, bob, 'Consider using bcrypt for passwords')")
  (meta-log-prolog-assert "comment(pr123, charlie, 'Add rate limiting to login endpoint')")
  (meta-log-prolog-assert "status(pr123, 'needs-changes')")

  (message "Pull Request: #123 - Add user authentication")
  (message "Author: Alice")
  (message "")

  ;; Query review status
  (message "Gathering review feedback...")
  (let ((comments (meta-log-prolog-query "comment(pr123, Reviewer, Comment)")))
    (message "Review comments:")
    (dolist (comment comments)
      (message "  - %s" comment)))
  (message "")

  ;; Find related team knowledge
  (message "Finding relevant team expertise...")
  (message "  - Bob has shared knowledge about Database Optimization")
  (message "  - Alice has expertise in Component Design")
  (message "  - Charlie can help with Container deployment")
  (message ""))

(defun demo-collab-team-metrics ()
  "Show team collaboration metrics."
  (interactive)
  (message "📊 Team Metrics")
  (message "")

  (message "Knowledge Base Statistics:")
  (message "  - Total shared knowledge nodes: %d" (meta-log-kg-node-count))
  (message "  - Knowledge connections: %d" (meta-log-kg-edge-count))
  (message "  - Team members: 3")
  (message "")

  (message "Collaboration Activity:")
  (message "  - Alice: 5 contributions (Frontend)")
  (message "  - Bob: 4 contributions (Backend)")
  (message "  - Charlie: 3 contributions (DevOps)")
  (message "")

  (message "Knowledge Coverage:")
  (message "  - Frontend: High")
  (message "  - Backend: High")
  (message "  - DevOps: Medium")
  (message "  - Testing: Low (needs attention)")
  (message ""))

(defun demo-collab-full ()
  "Run the complete team collaboration demo."
  (interactive)
  (message "")
  (message "════════════════════════════════════════")
  (message "  Team Knowledge Sharing Demo")
  (message "════════════════════════════════════════")
  (message "")

  (demo-collab-setup)
  (message "═══════════════════════════════════════")
  (demo-collab-knowledge-sharing)
  (message "═══════════════════════════════════════")
  (demo-collab-discovery)
  (message "═══════════════════════════════════════")
  (demo-collab-federated-learning)
  (message "═══════════════════════════════════════")
  (demo-collab-code-review)
  (message "═══════════════════════════════════════")
  (demo-collab-team-metrics)

  (message "")
  (message "✨ Demo complete!")
  (message "")
  (message "Key features demonstrated:")
  (message "  ✓ Federated knowledge sharing")
  (message "  ✓ Team expertise discovery")
  (message "  ✓ Multi-perspective learning")
  (message "  ✓ Collaborative code review")
  (message "  ✓ Team metrics and insights")
  (message "")
  (message "Next steps:")
  (message "  1. Connect your team with M-x meta-log-federation-connect")
  (message "  2. Share knowledge across team members")
  (message "  3. Discover expertise with semantic search")
  (message "  4. Build collective intelligence"))

;; Run demo
(when (called-interactively-p 'any)
  (demo-collab-full))

(provide '04-team-collaboration)
;;; 04-team-collaboration.el ends here
