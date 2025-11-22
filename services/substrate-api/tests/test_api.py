"""
Tests for Substrate API service.
"""

import pytest
from fastapi.testclient import TestClient
import base64

from app import app

client = TestClient(app)


def test_health_check():
    """Test health check endpoint."""
    response = client.get("/api/v1/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "healthy"
    assert data["service"] == "substrate-api"


def test_hash_endpoint():
    """Test hash computation endpoint."""
    test_data = b"\x01\x02\x03\x04"
    encoded = base64.b64encode(test_data).decode('utf-8')
    
    response = client.post(
        "/api/v1/substrate/hash",
        json={"data": encoded, "algorithm": "sha3-256"}
    )
    assert response.status_code == 200
    data = response.json()
    assert "hash" in data
    assert data["algorithm"] == "sha3-256"
    assert len(data["hash"]) == 64  # SHA3-256 hex length


def test_hash_sha3_512():
    """Test SHA3-512 hashing."""
    test_data = b"\x01\x02\x03\x04"
    encoded = base64.b64encode(test_data).decode('utf-8')
    
    response = client.post(
        "/api/v1/substrate/hash",
        json={"data": encoded, "algorithm": "sha3-512"}
    )
    assert response.status_code == 200
    data = response.json()
    assert data["algorithm"] == "sha3-512"
    assert len(data["hash"]) == 128  # SHA3-512 hex length


def test_hash_invalid_algorithm():
    """Test hash with invalid algorithm."""
    test_data = b"\x01\x02\x03\x04"
    encoded = base64.b64encode(test_data).decode('utf-8')
    
    response = client.post(
        "/api/v1/substrate/hash",
        json={"data": encoded, "algorithm": "md5"}
    )
    assert response.status_code == 400


def test_compress_endpoint():
    """Test compression endpoint."""
    test_data = b"x" * 1000  # 1000 bytes of 'x'
    encoded = base64.b64encode(test_data).decode('utf-8')
    
    response = client.post(
        "/api/v1/substrate/compress",
        json={"data": encoded, "algorithm": "deflate"}
    )
    assert response.status_code == 200
    data = response.json()
    assert "compressed" in data
    assert data["original_size"] == 1000
    assert data["compressed_size"] < 1000  # Should compress
    assert 0 < data["ratio"] < 1


def test_compress_invalid_algorithm():
    """Test compression with invalid algorithm."""
    test_data = b"\x01\x02\x03\x04"
    encoded = base64.b64encode(test_data).decode('utf-8')
    
    response = client.post(
        "/api/v1/substrate/compress",
        json={"data": encoded, "algorithm": "gzip"}
    )
    assert response.status_code == 400


if __name__ == "__main__":
    pytest.main([__file__, "-v"])

