#!/bin/bash
# Run Python tests with coverage reporting

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
COVERAGE_DIR="$PROJECT_ROOT/coverage/python"

echo "Running Python tests with coverage..."

# Create coverage directory
mkdir -p "$COVERAGE_DIR"

# Check if pytest-cov is installed
if ! python3 -c "import pytest_cov" 2>/dev/null; then
    echo "Error: pytest-cov not installed. Install with: pip install pytest-cov"
    exit 1
fi

# Run pytest with coverage
cd "$PROJECT_ROOT"
python3 -m pytest \
    --cov=services \
    --cov-report=html:"$COVERAGE_DIR/html" \
    --cov-report=term \
    --cov-report=xml:"$COVERAGE_DIR/coverage.xml" \
    --cov-report=json:"$COVERAGE_DIR/coverage.json" \
    tests/ services/ \
    -v

# Display summary
echo ""
echo "Coverage reports generated:"
echo "  - HTML: $COVERAGE_DIR/html/index.html"
echo "  - XML:  $COVERAGE_DIR/coverage.xml"
echo "  - JSON: $COVERAGE_DIR/coverage.json"

