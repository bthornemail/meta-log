#!/bin/bash
# Test FastAPI substrate service

set -e

echo "Testing Substrate API Service..."
echo ""

# Check if service is running
API_URL="${SUBSTRATE_API_URL:-http://localhost:8001}"

echo "Testing health endpoint..."
if curl -f -s "${API_URL}/api/v1/health" > /dev/null; then
    echo "✓ Health check passed"
else
    echo "✗ Health check failed - is the service running?"
    echo "  Start with: cd services/substrate-api && uvicorn app:app --port 8001"
    exit 1
fi

echo ""
echo "Testing hash endpoint..."
RESPONSE=$(curl -s -X POST "${API_URL}/api/v1/substrate/hash" \
     -H "Content-Type: application/json" \
     -d '{"data": "AQIDBA==", "algorithm": "sha3-256"}')

if echo "$RESPONSE" | grep -q '"hash"'; then
    echo "✓ Hash endpoint works"
    echo "  Response: $RESPONSE"
else
    echo "✗ Hash endpoint failed"
    echo "  Response: $RESPONSE"
    exit 1
fi

echo ""
echo "✓ All API tests passed!"

