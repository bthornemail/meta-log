#!/bin/bash
# Test script to verify meta-log installation

set -e

echo "🧪 Testing meta-log installation..."
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test results
PASSED=0
FAILED=0

# Helper functions
pass() {
    echo -e "${GREEN}✓${NC} $1"
    PASSED=$((PASSED + 1))
}

fail() {
    echo -e "${RED}✗${NC} $1"
    FAILED=$((FAILED + 1))
}

warn() {
    echo -e "${YELLOW}⚠${NC} $1"
}

# Test 1: Check Emacs is installed
echo "Checking Emacs..."
if command -v emacs &> /dev/null; then
    VERSION=$(emacs --version | head -n1)
    pass "Emacs found: $VERSION"
else
    fail "Emacs not found"
fi

# Test 2: Check meta-log files exist
echo ""
echo "Checking meta-log installation..."
if [ -f "$HOME/.meta-log/repo/meta-log.el" ]; then
    pass "meta-log files found in ~/.meta-log/repo"
elif [ -f "./meta-log.el" ]; then
    pass "meta-log files found in current directory"
else
    fail "meta-log files not found"
fi

# Test 3: Check new UX modules exist
echo ""
echo "Checking UX modules..."
UX_MODULES=("meta-log-setup.el" "meta-log-dashboard.el" "meta-log-chat.el" "meta-log-ingest.el")

for module in "${UX_MODULES[@]}"; do
    if [ -f "$HOME/.meta-log/repo/$module" ] || [ -f "./$module" ]; then
        pass "$module found"
    else
        fail "$module missing"
    fi
done

# Test 4: Check optional dependencies
echo ""
echo "Checking optional dependencies..."

if command -v ollama &> /dev/null; then
    pass "Ollama installed"
    if ollama list | grep -q "gemma2:2b"; then
        pass "gemma2:2b model downloaded"
    else
        warn "Ollama installed but gemma2:2b not downloaded (run: ollama pull gemma2:2b)"
    fi
else
    warn "Ollama not installed (optional, but recommended for local AI)"
fi

if command -v guile &> /dev/null; then
    pass "Guile (R5RS Scheme) installed"
else
    warn "Guile not installed (optional, for R5RS features)"
fi

# Test 5: Check Emacs can load meta-log
echo ""
echo "Testing meta-log loading in Emacs..."

cat > /tmp/test-meta-log.el <<'EOF'
(add-to-list 'load-path (expand-file-name "~/.meta-log/repo"))
(add-to-list 'load-path default-directory)

;; Try to load core modules
(condition-case err
    (progn
      (require 'meta-log-setup nil t)
      (require 'meta-log-dashboard nil t)
      (require 'meta-log-chat nil t)
      (require 'meta-log-ingest nil t)
      (message "SUCCESS: All UX modules loaded")
      (kill-emacs 0))
  (error
   (message "ERROR: %s" (error-message-string err))
   (kill-emacs 1)))
EOF

if emacs --batch -l /tmp/test-meta-log.el 2>&1 | grep -q "SUCCESS"; then
    pass "meta-log modules load successfully in Emacs"
else
    fail "meta-log modules failed to load"
    echo ""
    echo "Debug output:"
    emacs --batch -l /tmp/test-meta-log.el 2>&1 | tail -20
fi

rm -f /tmp/test-meta-log.el

# Test 6: Check configuration
echo ""
echo "Checking configuration..."

if [ -f "$HOME/.emacs.d/init.el" ]; then
    if grep -q "meta-log" "$HOME/.emacs.d/init.el"; then
        pass "meta-log configured in ~/.emacs.d/init.el"
    else
        warn "~/.emacs.d/init.el exists but doesn't mention meta-log"
    fi
fi

if [ -f "$HOME/.meta-log/config.el" ]; then
    pass "User config found at ~/.meta-log/config.el"
else
    warn "No user config yet (will be created on first run)"
fi

# Test 7: Check launchers
echo ""
echo "Checking launchers..."

if [ -f "$HOME/.local/share/applications/meta-log.desktop" ]; then
    pass "Desktop launcher installed (Linux)"
elif [ -f "$HOME/Desktop/meta-log.command" ]; then
    pass "Desktop launcher installed (macOS)"
elif [ -f "$HOME/.shortcuts/meta-log" ]; then
    pass "Termux widget shortcut installed"
else
    warn "No launcher found (optional)"
fi

# Summary
echo ""
echo "═══════════════════════════════════════"
echo "  Test Summary"
echo "═══════════════════════════════════════"
echo -e "  ${GREEN}Passed: $PASSED${NC}"
echo -e "  ${RED}Failed: $FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}🎉 All tests passed!${NC}"
    echo ""
    echo "Next steps:"
    echo "  1. Launch meta-log:"
    echo "     emacs --eval '(meta-log-dashboard)'"
    echo ""
    echo "  2. Or read the quick start guide:"
    echo "     cat ~/.meta-log/repo/QUICKSTART.md"
    echo ""
    exit 0
else
    echo -e "${RED}⚠ Some tests failed${NC}"
    echo ""
    echo "Try:"
    echo "  1. Re-run the installer:"
    echo "     curl -fsSL https://raw.githubusercontent.com/bthornemail/meta-log/main/install.sh | bash"
    echo ""
    echo "  2. Check the logs above for specific errors"
    echo ""
    echo "  3. Report issues at:"
    echo "     https://github.com/bthornemail/meta-log/issues"
    echo ""
    exit 1
fi
