;;; 03-code-analysis.el --- Demo: Code Analysis and Documentation

;; This demo shows how to use meta-log for analyzing codebases,
;; understanding code relationships, and generating documentation.

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'meta-log)
(require 'meta-log-knowledge-graph)

(defun demo-code-setup ()
  "Set up demo codebase analysis."
  (interactive)
  (message "💻 Demo: Code Analysis and Documentation")
  (message "")
  (message "Setting up code analysis environment...")

  ;; Initialize meta-log
  (meta-log-initialize)
  (message "✓ meta-log initialized")

  ;; Create sample code structure
  (let ((code-dir (expand-file-name "~/demo-codebase")))
    (unless (file-exists-p code-dir)
      (make-directory code-dir))

    ;; Create sample source files
    (with-temp-file (expand-file-name "user.py" code-dir)
      (insert "\"\"\"User management module.\"\"\"\n\n")
      (insert "class User:\n")
      (insert "    \"\"\"Represents a user in the system.\"\"\"\n")
      (insert "    def __init__(self, username, email):\n")
      (insert "        self.username = username\n")
      (insert "        self.email = email\n\n")
      (insert "    def validate(self):\n")
      (insert "        \"\"\"Validate user data.\"\"\"\n")
      (insert "        return len(self.username) > 0 and '@' in self.email\n"))

    (with-temp-file (expand-file-name "database.py" code-dir)
      (insert "\"\"\"Database connection module.\"\"\"\n\n")
      (insert "from user import User\n\n")
      (insert "class Database:\n")
      (insert "    \"\"\"Handles database operations.\"\"\"\n")
      (insert "    def __init__(self, connection_string):\n")
      (insert "        self.connection_string = connection_string\n\n")
      (insert "    def save_user(self, user: User):\n")
      (insert "        \"\"\"Save user to database.\"\"\"\n")
      (insert "        if user.validate():\n")
      (insert "            # Save to database\n")
      (insert "            pass\n"))

    (with-temp-file (expand-file-name "api.py" code-dir)
      (insert "\"\"\"API endpoints module.\"\"\"\n\n")
      (insert "from database import Database\n")
      (insert "from user import User\n\n")
      (insert "class API:\n")
      (insert "    \"\"\"REST API handler.\"\"\"\n")
      (insert "    def __init__(self, database: Database):\n")
      (insert "        self.db = database\n\n")
      (insert "    def create_user(self, username, email):\n")
      (insert "        \"\"\"Create new user endpoint.\"\"\"\n")
      (insert "        user = User(username, email)\n")
      (insert "        self.db.save_user(user)\n"))

    (message "✓ Created sample codebase in %s" code-dir)
    code-dir))

(defun demo-code-structure-analysis ()
  "Analyze code structure and dependencies."
  (interactive)
  (message "")
  (message "🔍 Analyzing Code Structure")
  (message "")

  ;; Build knowledge graph of code structure
  (message "Building code dependency graph...")

  ;; Add modules
  (meta-log-kg-add-node "user.py" "module")
  (meta-log-kg-add-node "database.py" "module")
  (meta-log-kg-add-node "api.py" "module")

  ;; Add classes
  (meta-log-kg-add-node "User" "class")
  (meta-log-kg-add-node "Database" "class")
  (meta-log-kg-add-node "API" "class")

  ;; Add relationships
  (meta-log-kg-add-edge "User" "defined-in" "user.py")
  (meta-log-kg-add-edge "Database" "defined-in" "database.py")
  (meta-log-kg-add-edge "API" "defined-in" "api.py")
  (meta-log-kg-add-edge "database.py" "imports" "user.py")
  (meta-log-kg-add-edge "api.py" "imports" "database.py")
  (meta-log-kg-add-edge "api.py" "imports" "user.py")
  (meta-log-kg-add-edge "Database" "depends-on" "User")
  (meta-log-kg-add-edge "API" "depends-on" "Database")

  (message "✓ Code structure mapped to knowledge graph")
  (message "")

  ;; Query dependencies
  (message "Q: What modules does api.py depend on?")
  (let ((deps (meta-log-kg-get-neighbors "api.py" "imports")))
    (message "A: api.py imports:")
    (dolist (dep deps)
      (message "  - %s" dep)))
  (message "")

  ;; Find dependency chains
  (message "Q: What's the dependency path from API to User?")
  (let ((path (meta-log-kg-find-path "API" "User")))
    (message "A: Dependency chain:")
    (message "  %s" (mapconcat 'identity path " → ")))
  (message ""))

(defun demo-code-documentation ()
  "Generate documentation from code analysis."
  (interactive)
  (message "📝 Generating Documentation")
  (message "")

  ;; Add function metadata
  (meta-log-prolog-assert "function(user_validate, 'user.py', 'User', 'Validates user data')")
  (meta-log-prolog-assert "function(db_save_user, 'database.py', 'Database', 'Saves user to database')")
  (meta-log-prolog-assert "function(api_create_user, 'api.py', 'API', 'Creates new user via API')")

  ;; Query documentation
  (message "Q: What functions are available in the User class?")
  (let ((results (meta-log-prolog-query "function(Name, 'user.py', 'User', Description)")))
    (message "A: User class functions:")
    (dolist (result results)
      (message "  - %s" result)))
  (message "")

  ;; Generate API documentation
  (message "Generating API documentation...")
  (let ((api-doc (meta-log-ask "Describe the API class and its purpose")))
    (message "API Documentation:")
    (message "%s" (or api-doc "The API class provides REST endpoints for user management.")))
  (message ""))

(defun demo-code-refactoring-insights ()
  "Provide refactoring suggestions based on code analysis."
  (interactive)
  (message "💡 Refactoring Insights")
  (message "")

  ;; Analyze complexity
  (message "Analyzing code complexity...")
  (meta-log-datalog-insert-fact "complexity" '("User" "low"))
  (meta-log-datalog-insert-fact "complexity" '("Database" "medium"))
  (meta-log-datalog-insert-fact "complexity" '("API" "medium"))

  ;; Check coupling
  (message "Checking module coupling...")
  (let ((api-deps (length (meta-log-kg-get-neighbors "api.py" "imports"))))
    (message "  - api.py has %d direct dependencies" api-deps)
    (when (> api-deps 2)
      (message "  ⚠️  Consider reducing coupling in api.py")))
  (message "")

  ;; Suggest improvements
  (message "Refactoring suggestions:")
  (message "  1. Consider extracting validation logic to a separate module")
  (message "  2. API class could benefit from dependency injection")
  (message "  3. Add error handling for database operations")
  (message ""))

(defun demo-code-search ()
  "Demonstrate semantic code search."
  (interactive)
  (message "🔎 Semantic Code Search")
  (message "")

  ;; Add semantic tags
  (meta-log-kg-add-edge "User" "has-concept" "authentication")
  (meta-log-kg-add-edge "Database" "has-concept" "persistence")
  (meta-log-kg-add-edge "API" "has-concept" "rest")
  (meta-log-kg-add-edge "API" "has-concept" "web-service")

  ;; Semantic search
  (message "Q: Find code related to 'web service'")
  (let ((results (meta-log-kg-search "web-service")))
    (message "A: Found components:")
    (dolist (result results)
      (message "  - %s" result)))
  (message "")

  ;; Cross-reference search
  (message "Q: What classes use the User class?")
  (let ((users (meta-log-kg-get-neighbors "User" "depends-on" t)))
    (message "A: Classes depending on User:")
    (dolist (user users)
      (message "  - %s" user)))
  (message ""))

(defun demo-code-full ()
  "Run the complete code analysis demo."
  (interactive)
  (message "")
  (message "════════════════════════════════════════")
  (message "  Code Analysis and Documentation Demo")
  (message "════════════════════════════════════════")
  (message "")

  (let ((code-dir (demo-code-setup)))
    (message "")
    (message "═══════════════════════════════════════")
    (demo-code-structure-analysis)
    (message "═══════════════════════════════════════")
    (demo-code-documentation)
    (message "═══════════════════════════════════════")
    (demo-code-refactoring-insights)
    (message "═══════════════════════════════════════")
    (demo-code-search)

    (message "")
    (message "✨ Demo complete!")
    (message "")
    (message "Key features demonstrated:")
    (message "  ✓ Code structure analysis")
    (message "  ✓ Dependency tracking")
    (message "  ✓ Automatic documentation generation")
    (message "  ✓ Refactoring insights")
    (message "  ✓ Semantic code search")
    (message "")
    (message "Your demo codebase is in: %s" code-dir)
    (message "")
    (message "Next steps:")
    (message "  1. Point meta-log at your own codebase")
    (message "  2. Use M-x meta-log-ask to query your code")
    (message "  3. Generate documentation with semantic understanding")))

;; Run demo
(when (called-interactively-p 'any)
  (demo-code-full))

(provide '03-code-analysis)
;;; 03-code-analysis.el ends here
