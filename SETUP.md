# Meta-Log Substrate System - Setup Guide

Complete dependency installation and setup instructions for the Meta-Log Substrate System (MLSS) with autonomy and awareness capabilities.

## System Dependencies

### Required System Packages

```bash
# Scheme/Guile (R5RS implementation)
sudo apt-get update
sudo apt-get install -y guile-3.0 guile-dev

# Python 3.9+ and pip
sudo apt-get install -y python3 python3-pip python3-venv

# FFmpeg (for video generation from 43-3D-Template-Video.md)
sudo apt-get install -y ffmpeg

# Node.js and npm (for frontend/automaton components)
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt-get install -y nodejs

# Build tools
sudo apt-get install -y build-essential git
```

### Verify Installations

```bash
guile --version    # Should be 3.0+
python3 --version  # Should be 3.9+
node --version     # Should be 20.x+
ffmpeg -version    # Should be 4.0+
```

## Python Dependencies

### Core FastAPI Services

All services use Python 3.9+ with FastAPI. Install dependencies for each service:

```bash
# Create virtual environment (recommended)
cd /home/main/meta-log
python3 -m venv venv
source venv/bin/activate

# Install base dependencies (shared across services)
pip install fastapi==0.104.1 uvicorn[standard]==0.24.0 pydantic==2.5.0

# Sensors API (for GPS, WiFi, BLE, motion sensors)
cd services/sensors-api
pip install -r requirements.txt
# Additional: websockets==12.0, opencv-python, Pillow (if needed)

# E8 API (for E8 lattice operations)
cd ../e8-api
pip install -r requirements.txt
# Additional: numpy==1.26.2, scipy==1.11.4, mpmath==1.3.0

# Vision API (for computer vision pipeline)
cd ../vision-api
pip install -r requirements.txt
# Additional: numpy==1.26.2, opencv-python==4.8.1.78, Pillow==10.1.0

# Quantum Simulation API
cd ../quantum-sim
pip install -r requirements.txt
# Additional: numpy==1.26.2

# Substrate API
cd ../substrate-api
pip install -r requirements.txt
```

### Combined Installation Script

```bash
#!/bin/bash
# install-python-deps.sh

cd /home/main/meta-log
source venv/bin/activate

# Install all service dependencies
for service in sensors-api e8-api vision-api quantum-sim substrate-api; do
    echo "Installing dependencies for $service..."
    cd services/$service
    pip install -r requirements.txt
    cd ../..
done

echo "All Python dependencies installed!"
```

## Node.js Dependencies

### Automaton Evolutions (Frontend)

```bash
cd /home/main/meta-log/automaton-evolutions
npm install
```

This installs dependencies for CanvasL protocol, WebRTC, WebAuthn, and other browser-based automatons.

## Scheme/Guile Setup

### Verify Guile Installation

```bash
# Test basic Scheme loading
guile -c "(display \"Guile is working!\n\")"

# Test MLSS engine loading
cd /home/main/meta-log
guile -c "(load \"scheme/r5rs-canvas-engine.scm\") (display \"MLSS engine loaded!\n\")"
```

### Guile Module Path

If you need to add custom paths:

```bash
export GUILE_LOAD_PATH="/home/main/meta-log/scheme:$GUILE_LOAD_PATH"
```

## Service Startup

### Start All Services

```bash
# Terminal 1: Sensors API
cd /home/main/meta-log/services/sensors-api
source ../../venv/bin/activate
uvicorn main:app --host 0.0.0.0 --port 8001

# Terminal 2: E8 API
cd /home/main/meta-log/services/e8-api
source ../../venv/bin/activate
uvicorn main:app --host 0.0.0.0 --port 8002

# Terminal 3: Vision API
cd /home/main/meta-log/services/vision-api
source ../../venv/bin/activate
uvicorn main:app --host 0.0.0.0 --port 8003

# Terminal 4: Quantum Simulation API
cd /home/main/meta-log/services/quantum-sim
source ../../venv/bin/activate
uvicorn main:app --host 0.0.0.0 --port 8004

# Terminal 5: Substrate API
cd /home/main/meta-log/services/substrate-api
source ../../venv/bin/activate
uvicorn main:app --host 0.0.0.0 --port 8005
```

### Service URLs

- Sensors API: http://localhost:8001
- E8 API: http://localhost:8002
- Vision API: http://localhost:8003
- Quantum Sim API: http://localhost:8004
- Substrate API: http://localhost:8005

## Testing Setup

### Run All Tests

```bash
cd /home/main/meta-log

# Mathematical validation tests
./tests/test-awareness-math.sh

# Autonomy tests
./tests/test-autonomy.sh

# Awareness validation tests
./tests/test-awareness.sh

# Full demo
./tests/demo-autonomy-awareness.sh
```

### Individual Module Tests

```bash
# Test consciousness modules
guile -c "(load \"scheme/r5rs-canvas-engine.scm\") (load \"scheme/consciousness/state.scm\") (display \"✓\n\")"

# Test action execution
guile -c "(load \"scheme/r5rs-canvas-engine.scm\") (load \"scheme/action/executor.scm\") (display \"✓\n\")"

# Test sensor integration
guile -c "(load \"scheme/r5rs-canvas-engine.scm\") (load \"scheme/sensors/manager.scm\") (display \"✓\n\")"
```

## Browser Dependencies (for W3C Media APIs)

The W3C Media Interaction features (from `44-W3C-Media-Interaction.md`) require browser APIs:

### Required Browser Features

- **getUserMedia**: Camera/microphone access (all modern browsers)
- **getDisplayMedia**: Screen capture (Chrome, Firefox, Safari 15.5+)
- **MediaRecorder**: Video recording (Chrome, Firefox, Safari limited)
- **Web Audio API**: Audio processing (universal support)
- **WebXR**: AR/VR (Chrome AR+VR, Firefox VR only, Safari AR via Quick Look)

### Testing Browser Support

```javascript
// Check media device support
if (navigator.mediaDevices && navigator.mediaDevices.getUserMedia) {
    console.log('✓ getUserMedia supported');
}

// Check WebXR support
if (navigator.xr) {
    navigator.xr.isSessionSupported('immersive-ar').then(supported => {
        console.log('AR supported:', supported);
    });
}
```

## Optional: Docker Setup

If you prefer containerized services:

```bash
# Build Docker images (if Dockerfiles exist)
cd /home/main/meta-log/services/substrate-api
docker build -t mlss-substrate-api .

# Run services in Docker
docker-compose up  # (if docker-compose.yml exists)
```

## Development Dependencies

### For Documentation (Jekyll/GitHub Pages)

```bash
cd /home/main/meta-log/docs
bundle install  # Installs Jekyll and dependencies from Gemfile
bundle exec jekyll serve  # Local preview at http://localhost:4000
```

### For TypeScript/JavaScript Development

```bash
# Install TypeScript globally (optional)
npm install -g typescript

# Install development dependencies
cd automaton-evolutions
npm install --save-dev typescript @types/node
```

## What Still Needs Implementation

Based on the research documents and current status:

### 1. Hopf Fibrations Integration (from RESEARCH-PLAN.md)

**Status**: Research complete, implementation pending

**Needed**:
- `scheme/topology/hopf.scm` - Core Hopf fibration implementations
- `scheme/geometry/s7-boundary.scm` - S⁷ boundary computation
- `scheme/consciousness/hopf-consciousness.scm` - Enhanced with explicit Hopf structure
- `scheme/geometry/merkaba.scm` - Merkaba pointer operations
- `services/topology/hopf_fibrations.py` - Python Hopf classes

**Priority**: HIGH (connects to existing E8 and consciousness modules)

### 2. W3C Media Interaction (from 44-W3C-Media-Interaction.md)

**Status**: Specification complete, implementation pending

**Needed**:
- Frontend TypeScript classes for MediaDeviceManager
- ScreenCaptureManager integration
- Web Audio processing pipeline
- WebXR session management
- Integration with A7 (WebAuthn), A9 (WebRTC), A10 (MQTT) automatons

**Priority**: MEDIUM (enhances user interaction capabilities)

### 3. 3D Template Video Generation (from 43-3D-Template-Video.md)

**Status**: Specification complete, implementation pending

**Needed**:
- GLB/GLTF manipulation pipeline
- Procedural geometry generators
- Frame capture system (headless WebGL)
- Video encoding integration (FFmpeg wrapper)
- Babylon.js integration

**Priority**: MEDIUM (completes visual synthesis pipeline)

### 4. Topological Concepts (from 45-Topological-Concepts.md)

**Status**: Research documentation complete, implementation pending

**Needed**:
- Pinch/branch point resolution algorithms
- Blow-up operations for singularity resolution
- Mobius strip and twisted bundle computations
- Orbifold structure for E8/W(E8) quotient

**Priority**: LOW (mathematical foundations, can be added incrementally)

## Quick Start Checklist

- [ ] Install system dependencies (Guile, Python, Node.js, FFmpeg)
- [ ] Create Python virtual environment
- [ ] Install Python dependencies for all services
- [ ] Install Node.js dependencies
- [ ] Verify Guile can load MLSS engine
- [ ] Start all FastAPI services
- [ ] Run test suite to verify installation
- [ ] (Optional) Set up Jekyll for documentation

## Troubleshooting

### Guile Module Not Found

```bash
export GUILE_LOAD_PATH="/home/main/meta-log/scheme:$GUILE_LOAD_PATH"
```

### Python Import Errors

```bash
# Ensure virtual environment is activated
source venv/bin/activate

# Reinstall dependencies
pip install --upgrade -r requirements.txt
```

### Service Port Conflicts

```bash
# Check what's using ports 8001-8005
sudo lsof -i :8001

# Change port in service main.py:
# uvicorn main:app --port 8006
```

### FFmpeg Not Found

```bash
# Install FFmpeg
sudo apt-get install -y ffmpeg

# Verify
ffmpeg -version
```

## Next Steps After Setup

1. **Run Tests**: Verify all components work
   ```bash
   ./tests/test-awareness-math.sh
   ./tests/test-autonomy.sh
   ```

2. **Start Services**: Launch all FastAPI services

3. **Explore Documentation**: Read `docs/MLSS_GUIDE.md` for usage examples

4. **Implement Missing Features**: Choose from the list above based on priority

5. **Contribute**: Add implementations for Hopf fibrations, media APIs, or video generation

---

**Last Updated**: 2025-01-XX  
**System Status**: Core MLSS operational, autonomy/awareness implemented, research features pending

