#!/bin/bash
# run-all-tests-with-deps.sh
# Test execution script with dependency checking
# Copyright (C) 2025 Meta-Log Research Group

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
REPORT_DIR="$SCRIPT_DIR/test-reports"
COVERAGE_DIR="$SCRIPT_DIR/coverage"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Create report directories
mkdir -p "$REPORT_DIR"
mkdir -p "$COVERAGE_DIR"

echo "╔════════════════════════════════════════════════════════════╗"
echo "║     meta-log Test Execution with Dependency Checking     ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Function to check if command exists
check_command() {
    if command -v "$1" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $1 found"
        return 0
    else
        echo -e "${RED}✗${NC} $1 not found"
        return 1
    fi
}

# Function to check Python package
check_python_package() {
    if python3 -c "import $1" 2>/dev/null; then
        echo -e "${GREEN}✓${NC} Python package: $1"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} Python package: $1 not installed (run: pip install $1)"
        return 1
    fi
}

# Check dependencies
echo "Checking dependencies..."
echo ""

MISSING_DEPS=0

# Check for Guile (Scheme)
if ! check_command "guile"; then
    echo "  Install: sudo apt-get install guile-3.0 (or equivalent)"
    MISSING_DEPS=$((MISSING_DEPS + 1))
fi

# Check for Python
if ! check_command "python3"; then
    echo "  Install: sudo apt-get install python3"
    MISSING_DEPS=$((MISSING_DEPS + 1))
fi

# Check for pytest
if ! check_python_package "pytest"; then
    MISSING_DEPS=$((MISSING_DEPS + 1))
fi

# Check for pytest-cov
if ! check_python_package "pytest_cov"; then
    MISSING_DEPS=$((MISSING_DEPS + 1))
fi

# Check for Emacs
if ! check_command "emacs"; then
    echo "  Install: sudo apt-get install emacs"
    MISSING_DEPS=$((MISSING_DEPS + 1))
fi

# Check for curl (for API tests)
if ! check_command "curl"; then
    echo "  Install: sudo apt-get install curl"
    MISSING_DEPS=$((MISSING_DEPS + 1))
fi

echo ""

if [ $MISSING_DEPS -gt 0 ]; then
    echo -e "${YELLOW}Warning: $MISSING_DEPS dependency(ies) missing. Some tests may be skipped.${NC}"
    echo ""
fi

# Run tests
echo "Running tests..."
echo ""

# 1. Scheme tests (if Guile available)
if command -v guile >/dev/null 2>&1; then
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "Scheme Tests"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    SCHEME_TEST_FILES=(
        "$PROJECT_ROOT/scheme/qstar/core.test.scm"
        "$PROJECT_ROOT/scheme/qstar/scoring.test.scm"
        "$PROJECT_ROOT/tests/test-prolog-interface.scm"
    )
    
    for test_file in "${SCHEME_TEST_FILES[@]}"; do
        if [ -f "$test_file" ]; then
            echo "Running: $(basename "$test_file")"
            if guile -s "$test_file" 2>&1 | tee "$REPORT_DIR/$(basename "$test_file" .scm).log"; then
                echo -e "${GREEN}✓ Passed${NC}"
            else
                echo -e "${RED}✗ Failed${NC}"
            fi
            echo ""
        fi
    done
else
    echo -e "${YELLOW}Skipping Scheme tests (Guile not available)${NC}"
    echo ""
fi

# 2. Python tests (if pytest available)
if python3 -c "import pytest" 2>/dev/null; then
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "Python Tests (with coverage)"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    # Find all test files
    PYTHON_TEST_DIRS=(
        "$PROJECT_ROOT/services/substrate-api/tests"
        "$PROJECT_ROOT/services/e8-api/tests"
        "$PROJECT_ROOT/services/vision-api"
    )
    
    for test_dir in "${PYTHON_TEST_DIRS[@]}"; do
        if [ -d "$test_dir" ]; then
            echo "Running tests in: $test_dir"
            cd "$test_dir/.."
            if pytest --cov=. --cov-report=html:"$COVERAGE_DIR/$(basename "$test_dir")" \
                     --cov-report=term \
                     -v 2>&1 | tee "$REPORT_DIR/python-$(basename "$test_dir").log"; then
                echo -e "${GREEN}✓ Passed${NC}"
            else
                echo -e "${RED}✗ Failed${NC}"
            fi
            echo ""
        fi
    done
else
    echo -e "${YELLOW}Skipping Python tests (pytest not available)${NC}"
    echo ""
fi

# 3. Emacs Lisp tests (if Emacs available)
if command -v emacs >/dev/null 2>&1; then
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "Emacs Lisp Tests"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    cd "$PROJECT_ROOT"
    if emacs --batch \
            --load "$SCRIPT_DIR/coverage-config.el" \
            --load "$SCRIPT_DIR/run-all-tests.el" \
            --eval "(run-all-tests)" \
            > "$REPORT_DIR/emacs-tests.log" 2>&1; then
        echo -e "${GREEN}✓ Tests completed${NC}"
        cat "$REPORT_DIR/emacs-tests.log"
    else
        echo -e "${RED}✗ Tests failed${NC}"
        cat "$REPORT_DIR/emacs-tests.log"
    fi
    echo ""
else
    echo -e "${YELLOW}Skipping Emacs Lisp tests (Emacs not available)${NC}"
    echo ""
fi

# Generate summary report
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test Execution Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Reports saved to: $REPORT_DIR"
echo "Coverage reports: $COVERAGE_DIR"
echo ""
echo "View coverage reports:"
echo "  - HTML: open $COVERAGE_DIR/*/index.html"
echo "  - Text: cat $REPORT_DIR/*.log"
echo ""

# Create test execution report
cat > "$REPORT_DIR/TEST-EXECUTION-REPORT.md" <<EOF
# Test Execution Report

**Date**: $(date)
**Script**: run-all-tests-with-deps.sh

## Dependencies Checked

- Guile: $(if command -v guile >/dev/null 2>&1; then echo "✓ Available"; else echo "✗ Missing"; fi)
- Python3: $(if command -v python3 >/dev/null 2>&1; then echo "✓ Available"; else echo "✗ Missing"; fi)
- pytest: $(if python3 -c "import pytest" 2>/dev/null; then echo "✓ Available"; else echo "✗ Missing"; fi)
- pytest-cov: $(if python3 -c "import pytest_cov" 2>/dev/null; then echo "✓ Available"; else echo "✗ Missing"; fi)
- Emacs: $(if command -v emacs >/dev/null 2>&1; then echo "✓ Available"; else echo "✗ Missing"; fi)

## Test Results

See individual log files in this directory for detailed results.

## Coverage Reports

Coverage reports are available in: $COVERAGE_DIR

EOF

echo -e "${GREEN}Test execution complete!${NC}"
echo ""

