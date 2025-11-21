# meta-log Test Suite

Comprehensive test coverage for meta-log functionality.

## Test Structure

```
tests/
├── README.md                    # This file
├── run-all-tests.el            # Master test runner
├── test-core.el                # Core functionality tests
├── test-llm.el                 # LLM integration tests
├── test-federation.el          # Federation tests
├── e2e/
│   └── test-workflow.el        # End-to-end workflow tests
└── test-*.el                   # Additional unit tests
```

## Running Tests

### Run All Tests
```bash
# From command line (batch mode)
emacs --batch -l tests/run-all-tests.el

# Or interactively
emacs -l tests/run-all-tests.el
```

### Run Specific Test Suite
```elisp
;; In Emacs
(load-file "tests/test-core.el")
(test-core-all)

;; Core tests
(load-file "tests/test-llm.el")
(test-llm-all)

;; E2E tests
(load-file "tests/e2e/test-workflow.el")
(test-e2e-all)
```

### Run Individual Test
```elisp
;; Load the test file
(load-file "tests/test-core.el")

;; Run specific test function
(test-core-initialization)
(test-core-prolog)
(test-core-knowledge-graph)
```

## Test Suites

### Unit Tests

#### `test-core.el` - Core Functionality
Tests the fundamental meta-log systems:
- System initialization
- Prolog engine
- Datalog engine
- M-expression parser and evaluator
- Knowledge graph operations

**Run:**
```elisp
(load-file "tests/test-core.el")
(test-core-all)
```

#### `test-llm.el` - LLM Integration
Tests AI/LLM functionality:
- Caching system
- Learning and adaptation
- Provider configuration
- API interactions

**Run:**
```elisp
(load-file "tests/test-llm.el")
(test-llm-all)
```

#### `test-federation.el` - Federation
Tests peer-to-peer and federation features:
- Cryptographic operations
- Identity management
- Protocol handlers
- Federation blackboard

**Run:**
```elisp
(load-file "tests/test-federation.el")
(test-federation-all)
```

### End-to-End Tests

#### `e2e/test-workflow.el` - Complete Workflows
Tests real-world usage scenarios:
- Knowledge ingestion workflow
- Query workflow (NL to results)
- Chat interaction workflow
- Learning and adaptation workflow

**Run:**
```elisp
(load-file "tests/e2e/test-workflow.el")
(test-e2e-all)
```

## Test Coverage

| Component | Unit Tests | E2E Tests | Status |
|-----------|------------|-----------|--------|
| Core System | ✓ | ✓ | Complete |
| Prolog Engine | ✓ | ✓ | Complete |
| Datalog Engine | ✓ | ✓ | Complete |
| M-Expression | ✓ | ✓ | Complete |
| Knowledge Graph | ✓ | ✓ | Complete |
| LLM Integration | ✓ | ✓ | Complete |
| Chat Interface | | ✓ | Partial |
| Federation | ✓ | | Complete |
| Ingest System | | ✓ | Partial |

## Writing Tests

### Test Function Template
```elisp
(defun test-my-feature ()
  "Test description."
  (message "Testing my feature...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test code
          (let ((result (my-function arg)))
            (unless (expected-condition result)
              (push "Error description" errors))))
      (error (push (format "Error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Test failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Test passed")
        t))))
```

### Test Suite Template
```elisp
(defun test-my-suite-all ()
  "Run all tests in my suite."
  (message "=== Running My Test Suite ===")
  (let ((results '()))
    (push (test-feature-1) results)
    (push (test-feature-2) results)
    (push (test-feature-3) results)

    (let ((passed (length (cl-remove-if-not 'identity results)))
          (total (length results)))
      (message "")
      (message "=== Test Results ===")
      (message "Passed: %d/%d" passed total)
      (if (= passed total)
          (progn
            (message "✓ All tests passed!")
            t)
        (progn
          (message "✗ Some tests failed")
          nil)))))
```

## Continuous Integration

### GitHub Actions Example
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

## Test Data

Tests create temporary data in:
- `/tmp/meta-log-test-*` - Temporary test files
- `*-test-*` buffers - Test buffers (cleaned up automatically)

## Debugging Tests

### Run with Debug Output
```elisp
;; Enable debug mode
(setq debug-on-error t)

;; Run test
(load-file "tests/test-core.el")
(test-core-all)
```

### Inspect Test State
```elisp
;; Check meta-log state
(message "Initialized: %s" meta-log--initialized-p)

;; Inspect knowledge graph
(message "Nodes: %d" (meta-log-kg-node-count))
(message "Edges: %d" (meta-log-kg-edge-count))
```

### Clean Test Environment
```elisp
;; Reset meta-log state between tests
(meta-log-reset)
(meta-log-initialize)
```

## Coverage Goals

Target coverage levels:
- **Unit Tests:** 80%+ of core functionality
- **Integration Tests:** All module interactions
- **E2E Tests:** All major user workflows
- **Regression Tests:** All fixed bugs

## Contributing Tests

When contributing:
1. Add tests for new features
2. Ensure all tests pass before submitting PR
3. Include both positive and negative test cases
4. Document test requirements and setup
5. Keep tests fast and focused

## Troubleshooting

**Tests hang:**
- Check for infinite loops in queries
- Reduce timeout values for debugging
- Run tests individually to isolate issue

**Tests fail intermittently:**
- Check for test isolation issues
- Ensure proper cleanup between tests
- Look for race conditions

**Missing dependencies:**
```elisp
;; Check for required modules
(require 'meta-log)
(require 'meta-log-llm nil t)
(require 'meta-log-federation nil t)
```

## Test Metrics

Track these metrics:
- Total tests: ~25+
- Pass rate: Target 100%
- Coverage: Target 80%+
- Run time: Target <30s for unit tests

## License

Tests are part of meta-log and use the same MIT License.
