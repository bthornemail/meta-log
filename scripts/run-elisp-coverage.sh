#!/bin/bash
# Run Emacs Lisp tests with coverage reporting using undercover.el

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
COVERAGE_DIR="$PROJECT_ROOT/coverage/elisp"

echo "Running Emacs Lisp tests with coverage..."

# Create coverage directory
mkdir -p "$COVERAGE_DIR"

# Check if Emacs is available
if ! command -v emacs &> /dev/null; then
    echo "Error: Emacs not found. Please install Emacs."
    exit 1
fi

# Create temporary Emacs script to run tests with coverage
TEMP_SCRIPT=$(mktemp)
cat > "$TEMP_SCRIPT" << 'EOF'
;; Load coverage configuration
(load-file "tests/coverage-config.el")

;; Load undercover if available
(condition-case nil
    (progn
      (require 'undercover)
      (message "undercover.el loaded"))
  (error
   (message "Warning: undercover.el not available. Install with: M-x package-install RET undercover RET")
   (kill-emacs 1)))

;; Configure undercover
(when (fboundp 'undercover)
  (setq undercover-force-coverage t)
  (setq undercover-report-format 'text)
  (setq undercover-report-dir (expand-file-name "coverage/elisp"))
  
  ;; Files to include
  (setq undercover-include-paths
        (list (expand-file-name "modules")
              (expand-file-name ".")))
  
  ;; Files to exclude
  (setq undercover-exclude-paths
        (list (expand-file-name "tests")
              (expand-file-name "dev-docs")
              (expand-file-name "docs"))))

;; Load ERT
(require 'ert)

;; Load test files
(let ((test-dir (expand-file-name "tests")))
  (when (file-directory-p test-dir)
    (dolist (file (directory-files test-dir t "\\.el$"))
      (when (string-match-p "test-" (file-name-nondirectory file))
        (load-file file)))))

;; Run tests
(ert-run-tests-batch-and-exit)
EOF

# Run Emacs with the script
cd "$PROJECT_ROOT"
emacs --batch --script "$TEMP_SCRIPT" 2>&1 | tee "$COVERAGE_DIR/test-output.txt"

# Clean up
rm -f "$TEMP_SCRIPT"

echo ""
echo "Coverage reports should be in: $COVERAGE_DIR"
echo "Check undercover.el documentation for report location."


