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

# Function to start FastAPI service
start_service() {
    local service_name=$1
    local service_dir=$2
    local port=$3
    
    if [ -d "$service_dir" ]; then
        echo "Starting $service_name on port $port..."
        cd "$service_dir"
        # Check if service is already running
        if curl -s "http://localhost:$port/health" >/dev/null 2>&1; then
            echo -e "${GREEN}✓ $service_name already running${NC}"
            return 0
        fi
        # Start service in background
        nohup uvicorn main:app --host 0.0.0.0 --port "$port" > "$REPORT_DIR/${service_name}.log" 2>&1 &
        local pid=$!
        echo "$pid" > "$REPORT_DIR/${service_name}.pid"
        # Wait for service to be ready
        for i in {1..10}; do
            if curl -s "http://localhost:$port/health" >/dev/null 2>&1; then
                echo -e "${GREEN}✓ $service_name started (PID: $pid)${NC}"
                return 0
            fi
            sleep 1
        done
        echo -e "${YELLOW}⚠ $service_name may not be ready yet${NC}"
        return 1
    else
        echo -e "${YELLOW}⚠ $service_name directory not found: $service_dir${NC}"
        return 1
    fi
}

# Function to stop FastAPI service
stop_service() {
    local service_name=$1
    local pid_file="$REPORT_DIR/${service_name}.pid"
    if [ -f "$pid_file" ]; then
        local pid=$(cat "$pid_file")
        if kill "$pid" 2>/dev/null; then
            echo -e "${GREEN}✓ Stopped $service_name (PID: $pid)${NC}"
        fi
        rm -f "$pid_file"
    fi
}

# Start required services
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Starting Required Services"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Check if uvicorn is available
if python3 -c "import uvicorn" 2>/dev/null; then
    start_service "e8-api" "$PROJECT_ROOT/services/e8-api" 8000
    start_service "vision-api" "$PROJECT_ROOT/services/vision-api" 8001
    start_service "quantum-simulation" "$PROJECT_ROOT/services/quantum-simulation" 8002
    start_service "sensors-api" "$PROJECT_ROOT/services/sensors-api" 8003
    echo ""
else
    echo -e "${YELLOW}⚠ uvicorn not available, skipping service startup${NC}"
    echo ""
fi

# 2. Python tests (if pytest available)
if python3 -c "import pytest" 2>/dev/null; then
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "Python Tests (with coverage)"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    # Use the coverage script if available
    if [ -f "$PROJECT_ROOT/scripts/run-python-coverage.sh" ]; then
        echo "Using coverage script..."
        "$PROJECT_ROOT/scripts/run-python-coverage.sh" 2>&1 | tee "$REPORT_DIR/python-coverage.log"
    else
        # Fallback: Find all test files
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
    fi
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

# Stop services
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Stopping Services"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
stop_service "e8-api"
stop_service "vision-api"
stop_service "quantum-simulation"
stop_service "sensors-api"
echo ""

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

# Count test results
SCHEME_PASSED=0
SCHEME_FAILED=0
PYTHON_PASSED=0
PYTHON_FAILED=0
ELISP_PASSED=0
ELISP_FAILED=0

# Count from log files (simplified - would need more sophisticated parsing)
if [ -f "$REPORT_DIR/emacs-tests.log" ]; then
    if grep -q "passed" "$REPORT_DIR/emacs-tests.log"; then
        ELISP_PASSED=1
    fi
fi

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
- uvicorn: $(if python3 -c "import uvicorn" 2>/dev/null; then echo "✓ Available"; else echo "✗ Missing"; fi)

## Services Started

- E8 API: $(if curl -s http://localhost:8000/health >/dev/null 2>&1; then echo "✓ Running"; else echo "✗ Not running"; fi)
- Vision API: $(if curl -s http://localhost:8001/health >/dev/null 2>&1; then echo "✓ Running"; else echo "✗ Not running"; fi)
- Quantum Simulation: $(if curl -s http://localhost:8002/health >/dev/null 2>&1; then echo "✓ Running"; else echo "✗ Not running"; fi)
- Sensors API: $(if curl -s http://localhost:8003/health >/dev/null 2>&1; then echo "✓ Running"; else echo "✗ Not running"; fi)

## Test Results

See individual log files in this directory for detailed results.

### Test Files Executed

- Scheme tests: See \`*.scm.log\` files
- Python tests: See \`python-*.log\` files
- Emacs Lisp tests: See \`emacs-tests.log\`

## Coverage Reports

Coverage reports are available in: $COVERAGE_DIR

- Python: \`$COVERAGE_DIR/python/html/index.html\`
- Emacs Lisp: Check \`coverage/elisp/\` directory

## Next Steps

1. Review test logs for failures
2. Check coverage reports for uncovered code
3. Fix any failing tests
4. Improve coverage for low-coverage areas

EOF

echo -e "${GREEN}Test execution complete!${NC}"
echo ""
echo "View detailed report: $REPORT_DIR/TEST-EXECUTION-REPORT.md"
echo ""

