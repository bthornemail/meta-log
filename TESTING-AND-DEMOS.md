# Testing and Demos Guide

Complete guide to meta-log's test suite and interactive demos.

## Quick Links

- **Run Tests:** `emacs --batch -l tests/run-all-tests.el`
- **Run Demos:** `emacs -l demos/run-demos.el`
- **Test Documentation:** [tests/README.md](tests/README.md)
- **Demo Documentation:** [demos/README.md](demos/README.md)

---

## Project Structure

```
meta-log/
├── meta-log.el              # Main entry point
├── modules/                 # All module files (41 modules)
│   ├── meta-log-core.el
│   ├── meta-log-prolog.el
│   ├── meta-log-llm.el
│   └── ...
├── tests/                   # Test suite
│   ├── README.md           # Test documentation
│   ├── run-all-tests.el    # Master test runner
│   ├── test-core.el        # Core functionality tests
│   ├── test-llm.el         # LLM integration tests
│   ├── test-federation.el  # Federation tests
│   ├── test-*.el           # Additional unit tests
│   └── e2e/                # End-to-end tests
│       └── test-workflow.el
└── demos/                   # Interactive demonstrations
    ├── README.md           # Demo documentation
    ├── run-demos.el        # Interactive demo launcher
    ├── 01-personal-knowledge-base.el
    ├── 02-research-assistant.el
    ├── 03-code-analysis.el
    └── 04-team-collaboration.el
```

---

## Testing

### Running Tests

#### All Tests
```bash
# Batch mode (CI/CD)
emacs --batch -l tests/run-all-tests.el

# Interactive mode
emacs -l tests/run-all-tests.el
```

#### Specific Test Suite
```elisp
;; Core functionality
(load-file "tests/test-core.el")
(test-core-all)

;; LLM integration
(load-file "tests/test-llm.el")
(test-llm-all)

;; End-to-end workflows
(load-file "tests/e2e/test-workflow.el")
(test-e2e-all)
```

### Test Coverage

| Category | Suites | Tests | Coverage |
|----------|--------|-------|----------|
| Unit Tests | 3 | 15+ | Core modules |
| Integration Tests | 1 | 5+ | Module interactions |
| E2E Tests | 1 | 4+ | User workflows |
| **Total** | **5** | **25+** | **~80%** |

### What's Tested

✅ **Core Functionality**
- System initialization
- Prolog engine (facts, rules, queries)
- Datalog engine (fact insertion, queries)
- M-expression parsing and evaluation
- Knowledge graph operations

✅ **LLM Integration**
- Caching system
- Learning and adaptation
- Provider configuration
- Multi-provider support

✅ **Federation**
- Cryptographic operations (BIP32/39/44)
- Identity management
- Protocol handlers (WebRTC, MQTT)
- Federation blackboard

✅ **End-to-End Workflows**
- Knowledge ingestion (files → graph)
- Query workflow (natural language → results)
- Chat interaction
- Learning and adaptation

### Test Output Example

```
╔════════════════════════════════════════════╗
║     meta-log Complete Test Suite          ║
╚════════════════════════════════════════════╝

📦 Running Unit Tests...

=== Running Core Test Suite ===
Testing core initialization...
✓ Core initialization tests passed
Testing Prolog engine...
✓ Prolog tests passed
Testing Datalog engine...
✓ Datalog tests passed
...

╔════════════════════════════════════════════╗
║           Final Test Results               ║
╚════════════════════════════════════════════╝

✓ Core Tests
✓ LLM Tests
✓ Federation Tests
✓ E2E Workflow Tests

Total Suites: 4
Passed: 4
Failed: 0
Time: 12.34s

🎉 All test suites passed!
```

---

## Demos

### Running Demos

#### Interactive Launcher
```bash
# Start interactive demo menu
emacs -l demos/run-demos.el
```

The launcher provides:
- Menu-driven demo selection
- Demo descriptions
- Quick start guide
- Sequential or individual demo execution

#### Direct Demo Execution
```elisp
;; Personal Knowledge Base
(load-file "demos/01-personal-knowledge-base.el")
(demo-personal-kb-full)

;; Research Assistant
(load-file "demos/02-research-assistant.el")
(demo-research-full)

;; Code Analysis
(load-file "demos/03-code-analysis.el")
(demo-code-full)

;; Team Collaboration
(load-file "demos/04-team-collaboration.el")
(demo-collab-full)
```

### Available Demos

#### 1. Personal Knowledge Base
**Audience:** Students, researchers, writers, knowledge workers

**What you'll learn:**
- Import notes from Org files
- Build a knowledge graph automatically
- Query with natural language
- Find connections between concepts
- Semantic search

**Duration:** 5-10 minutes

---

#### 2. Research Assistant
**Audience:** Academics, researchers, continuous learners

**What you'll learn:**
- AI that learns from interactions
- Managing research papers and citations
- Interactive chat interface
- Knowledge synthesis across topics
- Research query capabilities

**Duration:** 10-15 minutes

---

#### 3. Code Analysis
**Audience:** Developers, technical leads, code reviewers

**What you'll learn:**
- Analyze code structure
- Track dependencies
- Generate documentation
- Get refactoring insights
- Semantic code search

**Duration:** 10-15 minutes

---

#### 4. Team Collaboration
**Audience:** Development teams, distributed teams, open source

**What you'll learn:**
- Federated knowledge sharing
- Team expertise discovery
- Multi-perspective learning
- Collaborative code review
- Team metrics and insights

**Duration:** 10-15 minutes

---

### Demo Output Example

```
════════════════════════════════════════
  Personal Knowledge Base Demo
════════════════════════════════════════

🚀 Demo: Personal Knowledge Base

Setting up your personal knowledge base...
✓ meta-log initialized
✓ Created sample notes in ~/demo-notes
✓ Notes ingested

═══════════════════════════════════════

📚 Querying Your Knowledge Base

Q: What do I know about functional programming?
A: Found 5 related entries
  - Functional Programming
  - Lisp Family
  - Scheme
  - Common Lisp
  - Clojure

═══════════════════════════════════════

🔍 Knowledge Graph Insights

Finding connections between concepts...
Concepts related to Prolog:
  - Logic Programming
  - Declarative Programming
  - The Art of Prolog

Knowledge Base Statistics:
  Total nodes: 25
  Total edges: 38
  Topics: 8

✨ Demo complete!
```

---

## For Developers

### Adding Tests

1. **Create test file:** `tests/test-myfeature.el`
2. **Write test functions:** Follow existing patterns
3. **Add to test runner:** Update `run-all-tests.el`
4. **Document:** Add to test README

Example test function:
```elisp
(defun test-my-feature ()
  "Test my new feature."
  (message "Testing my feature...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((result (my-new-function)))
            (unless result
              (push "Feature should work" errors))))
      (error (push (format "Error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Test failed: %s"
                   (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Test passed")
        t))))
```

### Adding Demos

1. **Create demo file:** `demos/05-my-demo.el`
2. **Implement demo functions:** Setup, execution, teardown
3. **Add to launcher:** Update `run-demos.el`
4. **Document:** Add to demo README

Demo structure:
```elisp
(defun demo-my-demo-setup ()
  "Set up demo environment."
  (meta-log-initialize)
  ;; Setup code
  )

(defun demo-my-demo-showcase ()
  "Demonstrate key features."
  ;; Demo code
  )

(defun demo-my-demo-full ()
  "Run complete demo."
  (interactive)
  (demo-my-demo-setup)
  (demo-my-demo-showcase)
  (message "Demo complete!"))
```

---

## CI/CD Integration

### GitHub Actions

```yaml
name: Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Emacs
        run: sudo apt-get install -y emacs
      - name: Run Tests
        run: emacs --batch -l tests/run-all-tests.el
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit
echo "Running tests..."
emacs --batch -l tests/run-all-tests.el
if [ $? -ne 0 ]; then
    echo "Tests failed. Commit aborted."
    exit 1
fi
```

---

## Best Practices

### Testing
- ✅ Write tests for all new features
- ✅ Keep tests fast and focused
- ✅ Clean up test data
- ✅ Use descriptive test names
- ✅ Test both success and failure cases

### Demos
- ✅ Make demos self-contained
- ✅ Include clear explanations
- ✅ Show real-world use cases
- ✅ Clean up demo data
- ✅ Provide next steps

---

## Troubleshooting

### Tests Fail
```elisp
;; Enable debug mode
(setq debug-on-error t)

;; Check meta-log state
(meta-log-reset)
(meta-log-initialize)

;; Run individual test
(test-core-initialization)
```

### Demos Don't Run
```elisp
;; Ensure meta-log is loaded
(require 'meta-log)
(meta-log-initialize)

;; Check for missing modules
(require 'meta-log-llm nil t)
(require 'meta-log-federation nil t)
```

---

## Contributing

We welcome contributions to tests and demos!

**Test contributions:**
- Increase coverage of existing modules
- Add regression tests for bugs
- Create integration tests
- Improve E2E scenarios

**Demo contributions:**
- New use case demonstrations
- Domain-specific examples
- Integration showcases
- Tutorial-style demos

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

---

## Resources

- **Test Documentation:** [tests/README.md](tests/README.md)
- **Demo Documentation:** [demos/README.md](demos/README.md)
- **Main Documentation:** [README.md](README.md)
- **API Reference:** [docs/API.md](docs/API.md)

---

## License

Tests and demos are part of meta-log and use the MIT License.
