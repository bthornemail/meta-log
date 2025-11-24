---
layout: default
title: Setup Guide
nav_order: 10
description: "Complete setup instructions for meta-log"
permalink: /guides/SETUP
---

# Setup Guide

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
# install-dependencies.sh

set -e

echo "Installing meta-log dependencies..."

# Check for system dependencies
command -v guile >/dev/null 2>&1 || { echo "Error: guile not found. Install with: sudo apt-get install guile-3.0"; exit 1; }
command -v python3 >/dev/null 2>&1 || { echo "Error: python3 not found"; exit 1; }

# Create virtual environment
python3 -m venv venv
source venv/bin/activate

# Install all service dependencies
for service in services/*/requirements.txt; do
    if [ -f "$service" ]; then
        echo "Installing dependencies for $(dirname $service)..."
        pip install -r "$service"
    fi
done

echo "✓ Dependencies installed successfully"
```

## Docker Setup

### Using Docker Compose

```bash
cd docker
docker-compose up -d
```

This starts:
- E8 API (port 8000)
- Substrate API (port 8001)
- MQTT Broker (port 1883)
- Meta-log service (port 8080)

### Using Kubernetes

```bash
# Apply namespace
kubectl apply -f k8s/namespace.yaml

# Apply E8 API
kubectl apply -f k8s/e8-api-configmap.yaml
kubectl apply -f k8s/e8-api-pvc.yaml
kubectl apply -f k8s/e8-api-deployment.yaml
kubectl apply -f k8s/e8-api-service.yaml

# Apply MQTT Broker
kubectl apply -f k8s/mqtt-broker-deployment.yaml
kubectl apply -f k8s/mqtt-broker-service.yaml

# Apply Meta-log
kubectl apply -f k8s/meta-log-deployment.yaml
kubectl apply -f k8s/meta-log-service.yaml

# Or use kustomize
kubectl apply -k k8s/
```

## Verification

### Test Installation

```bash
# Run quick demo
./tests/demo-mlss-quick.sh

# Run autonomy/awareness demo
./tests/demo-autonomy-awareness.sh

# Run tests
./tests/test-awareness-math.sh
```

### Check Services

```bash
# E8 API
curl http://localhost:8000/api/v1/health

# Substrate API
curl http://localhost:8001/api/v1/health

# Sensors API (if running)
curl http://localhost:8002/api/v1/health
```

## Troubleshooting

### Guile Not Found

```bash
sudo apt-get install guile-3.0 guile-dev
```

### Python Dependencies Fail

```bash
# Use virtual environment
python3 -m venv venv
source venv/bin/activate
pip install --upgrade pip
pip install -r requirements.txt
```

### Services Won't Start

```bash
# Check logs
docker-compose logs

# Or for Kubernetes
kubectl logs -n meta-log deployment/meta-log
```

## Next Steps

- [Getting Started](GETTING-STARTED) - Quick start guide
- [System Status](../STATUS) - Current implementation status
- [Core Concepts](../CORE-CONCEPTS) - Understand the system

---

**Need help?** Check [System Status](../STATUS) or [Getting Started](GETTING-STARTED) for more information.
