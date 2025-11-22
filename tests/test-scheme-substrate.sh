#!/bin/bash
# Quick test script for Scheme substrate modules

set -e

echo "Testing Scheme Substrate Modules..."
echo ""

# Test if Guile can load the modules
if command -v guile &> /dev/null; then
    echo "Testing module loading..."
    guile -c "(load \"scheme/r5rs-canvas-engine.scm\") (display \"✓ Modules loaded successfully\n\")" || {
        echo "✗ Module loading failed"
        exit 1
    }
    
    echo "Testing basic functions..."
    guile -c "
    (load \"scheme/r5rs-canvas-engine.scm\")
    (let ((result (substrate-create-memory '(1 2 3 4) '((encoding . \"raw\")))))
      (if (and (list? result) (= (length result) 2))
          (display \"✓ substrate-create-memory works\n\")
          (begin (display \"✗ substrate-create-memory failed\n\") (exit 1))))
    " || {
        echo "✗ Function test failed"
        exit 1
    }
    
    echo ""
    echo "✓ All Scheme tests passed!"
else
    echo "Guile not found. Skipping Scheme tests."
    echo "Install Guile 3.0+ to run Scheme tests."
fi

