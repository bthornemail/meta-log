#!/bin/bash
# Meta-Log Substrate System - Dependency Installation Script

set -e

echo "=========================================="
echo "Meta-Log Substrate System - Setup"
echo "=========================================="
echo ""

# Check if we're in the right directory
if [ ! -f "scheme/r5rs-canvas-engine.scm" ]; then
    echo "Error: Must run from meta-log root directory"
    exit 1
fi

# System dependencies check
echo "Checking system dependencies..."
echo ""

if ! command -v guile &> /dev/null; then
    echo "✗ Guile not found. Install with: sudo apt-get install guile-3.0"
    exit 1
else
    echo "✓ Guile: $(guile --version | head -1)"
fi

if ! command -v python3 &> /dev/null; then
    echo "✗ Python3 not found. Install with: sudo apt-get install python3 python3-pip"
    exit 1
else
    echo "✓ Python: $(python3 --version)"
fi

if ! command -v node &> /dev/null; then
    echo "✗ Node.js not found. Install from: https://nodejs.org/"
    exit 1
else
    echo "✓ Node.js: $(node --version)"
fi

if ! command -v ffmpeg &> /dev/null; then
    echo "⚠ FFmpeg not found (optional, for video generation)"
    echo "  Install with: sudo apt-get install ffmpeg"
else
    echo "✓ FFmpeg: $(ffmpeg -version | head -1 | cut -d' ' -f3)"
fi

echo ""
echo "=========================================="
echo "Installing Python Dependencies"
echo "=========================================="
echo ""

# Create virtual environment if it doesn't exist
if [ ! -d "venv" ]; then
    echo "Creating Python virtual environment..."
    python3 -m venv venv
fi

# Activate virtual environment
source venv/bin/activate

# Upgrade pip
echo "Upgrading pip..."
pip install --upgrade pip > /dev/null

# Install base dependencies
echo "Installing base FastAPI dependencies..."
pip install fastapi==0.104.1 uvicorn[standard]==0.24.0 pydantic==2.5.0 > /dev/null

# Install service-specific dependencies
echo ""
echo "Installing service dependencies..."

# Sensors API
if [ -f "services/sensors-api/requirements.txt" ]; then
    echo "  - Sensors API..."
    pip install -q -r services/sensors-api/requirements.txt
    # Add missing dependencies
    pip install -q opencv-python Pillow numpy || true
fi

# E8 API
if [ -f "services/e8-api/requirements.txt" ]; then
    echo "  - E8 API..."
    pip install -q -r services/e8-api/requirements.txt
fi

# Vision API
if [ -f "services/vision-api/requirements.txt" ]; then
    echo "  - Vision API..."
    pip install -q -r services/vision-api/requirements.txt
fi

# Quantum Sim API
if [ -f "services/quantum-sim/requirements.txt" ]; then
    echo "  - Quantum Sim API..."
    pip install -q -r services/quantum-sim/requirements.txt
fi

# Substrate API
if [ -f "services/substrate-api/requirements.txt" ]; then
    echo "  - Substrate API..."
    pip install -q -r services/substrate-api/requirements.txt
fi

echo ""
echo "✓ Python dependencies installed!"
echo ""

# Verify installation
echo "Verifying Python packages..."
python3 -c "import fastapi; print('  ✓ fastapi:', fastapi.__version__)" 2>/dev/null || echo "  ✗ fastapi: FAILED"
python3 -c "import numpy; print('  ✓ numpy:', numpy.__version__)" 2>/dev/null || echo "  ✗ numpy: FAILED"
python3 -c "import cv2; print('  ✓ opencv-python:', cv2.__version__)" 2>/dev/null || echo "  ⚠ opencv-python: Not installed (optional)"

echo ""
echo "=========================================="
echo "Installing Node.js Dependencies"
echo "=========================================="
echo ""

if [ -f "automaton-evolutions/package.json" ]; then
    cd automaton-evolutions
    if [ ! -d "node_modules" ]; then
        echo "Installing npm packages..."
        npm install
    else
        echo "✓ Node.js dependencies already installed"
    fi
    cd ..
else
    echo "⚠ No package.json found in automaton-evolutions/"
fi

echo ""
echo "=========================================="
echo "Testing MLSS Core"
echo "=========================================="
echo ""

# Test Guile loading
echo "Testing Guile MLSS engine..."
if guile -c "(load \"scheme/r5rs-canvas-engine.scm\") (display \"✓ MLSS engine loads successfully\n\")" 2>&1 | grep -q "✓"; then
    echo "  ✓ MLSS engine loads successfully"
else
    echo "  ⚠ MLSS engine test (check output above)"
fi

echo ""
echo "=========================================="
echo "Installation Complete!"
echo "=========================================="
echo ""
echo "Next steps:"
echo "  1. Activate virtual environment: source venv/bin/activate"
echo "  2. Start services (see SETUP.md for details)"
echo "  3. Run tests: ./tests/test-awareness-math.sh"
echo "  4. Read SETUP.md for complete setup guide"
echo ""
echo "To activate virtual environment in future sessions:"
echo "  source venv/bin/activate"
echo ""

