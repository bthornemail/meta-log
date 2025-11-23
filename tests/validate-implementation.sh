#!/bin/bash
# Validate MLSS implementation structure

set -e

echo "=========================================="
echo "MLSS Implementation Validation"
echo "=========================================="
echo ""

ERRORS=0
WARNINGS=0

# Check Scheme modules
echo "Checking Scheme modules..."
SCHEME_FILES=(
    "scheme/substrate/runtime.scm"
    "scheme/substrate/binary.scm"
    "scheme/substrate/provenance.scm"
    "scheme/substrate/content-address.scm"
    "scheme/substrate/canvasl.scm"
    "scheme/substrate/prolog-interface.scm"
    "scheme/substrate/waveform.scm"
    "scheme/substrate/wdl.scm"
    "scheme/substrate/cdmp.scm"
    "scheme/qstar/core.scm"
    "scheme/qstar/scoring.scm"
    "scheme/qstar/a-star.scm"
    "scheme/consciousness/state.scm"
    "scheme/physics/quantum.scm"
    "scheme/physics/gr.scm"
    "scheme/r5rs-canvas-engine.scm"
)

for file in "${SCHEME_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "  ✓ $file"
    else
        echo "  ✗ $file MISSING"
        ((ERRORS++))
    fi
done

# Check Emacs modules
echo ""
echo "Checking Emacs Lisp modules..."
ELISP_FILES=(
    "modules/meta-log-substrate-runtime.el"
    "modules/meta-log-binary-substrate.el"
    "modules/meta-log-provenance.el"
)

for file in "${ELISP_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "  ✓ $file"
    else
        echo "  ✗ $file MISSING"
        ((ERRORS++))
    fi
done

# Check FastAPI service
echo ""
echo "Checking FastAPI service..."
API_FILES=(
    "services/substrate-api/app.py"
    "services/substrate-api/requirements.txt"
    "services/substrate-api/Dockerfile"
    "services/substrate-api/tests/test_api.py"
)

for file in "${API_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "  ✓ $file"
    else
        echo "  ✗ $file MISSING"
        ((ERRORS++))
    fi
done

# Check documentation
echo ""
echo "Checking documentation..."
DOC_FILES=(
    "dev-docs/INTEGRATION-MLSS.md"
    "dev-docs/MLSS-IMPLEMENTATION-SUMMARY.md"
    "scheme/README.md"
    "scheme/QUICK-START.md"
    "tests/README-TESTING.md"
)

for file in "${DOC_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "  ✓ $file"
    else
        echo "  ✗ $file MISSING"
        ((WARNINGS++))
    fi
done

# Check Docker integration
echo ""
echo "Checking Docker integration..."
if grep -q "substrate-api" docker/docker-compose.yml 2>/dev/null; then
    echo "  ✓ substrate-api in docker-compose.yml"
else
    echo "  ✗ substrate-api not in docker-compose.yml"
    ((ERRORS++))
fi

# Check meta-log.el integration
echo ""
echo "Checking meta-log.el integration..."
if grep -q "meta-log-substrate-runtime" meta-log.el 2>/dev/null; then
    echo "  ✓ Substrate modules referenced in meta-log.el"
else
    echo "  ⚠ Substrate modules not referenced in meta-log.el"
    ((WARNINGS++))
fi

# Summary
echo ""
echo "=========================================="
echo "Validation Summary"
echo "=========================================="
echo "Errors: $ERRORS"
echo "Warnings: $WARNINGS"

if [ $ERRORS -eq 0 ]; then
    echo ""
    echo "✓ All critical components present!"
    echo ""
    echo "Next steps:"
    echo "  1. Install dependencies (Guile, pytest, FastAPI)"
    echo "  2. Run: ./tests/run-substrate-tests.sh"
    exit 0
else
    echo ""
    echo "✗ Validation failed with $ERRORS errors"
    exit 1
fi

