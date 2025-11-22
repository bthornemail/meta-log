"""
E8 API Integration Tests
Tests for Python FastAPI service
"""

import pytest
import requests
import numpy as np
from fastapi.testclient import TestClient

from app import app

client = TestClient(app)


def test_health_check():
    """Test health check endpoint"""
    response = client.get("/api/v1/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "healthy"
    assert "e8_roots" in data
    assert "theta_coefficients" in data


def test_get_e8_point():
    """Test getting E8 point from BIP32 path"""
    response = client.get("/api/v1/e8/point/m/44'/0'/0'/0/0")
    assert response.status_code == 200
    data = response.json()
    assert "coords" in data
    assert len(data["coords"]) == 8
    assert data["bip32_path"] == "m/44'/0'/0'/0/0"
    assert "norm_squared" in data
    assert "is_root" in data


def test_verify_frbac_delegation():
    """Test FRBAC delegation verification"""
    response = client.post(
        "/api/v1/e8/verify",
        json={
            "master_path": "m/44'/0'/0'",
            "delegate_path": "m/44'/0'/0'/0/0"
        }
    )
    assert response.status_code == 200
    assert isinstance(response.json(), bool)


def test_shortest_path():
    """Test shortest path computation"""
    response = client.post(
        "/api/v1/e8/path",
        json={
            "from_path": "m/44'/0'/0'",
            "to_path": "m/44'/0'/0'/0/0",
            "max_steps": 10
        }
    )
    assert response.status_code == 200
    data = response.json()
    assert "path" in data
    assert "distance" in data
    assert len(data["path"]) > 0


def test_weyl_orbit():
    """Test Weyl orbit computation"""
    response = client.post(
        "/api/v1/e8/weyl-orbit",
        json={
            "path": "m/44'/0'/0'",
            "max_size": 10
        }
    )
    assert response.status_code == 200
    data = response.json()
    assert "orbit" in data
    assert "size" in data
    assert data["size"] <= 10


def test_distance_metrics():
    """Test distance metrics computation"""
    response = client.post(
        "/api/v1/e8/distance",
        json={
            "master_path": "m/44'/0'/0'",
            "delegate_path": "m/44'/0'/0'/0/0"
        }
    )
    assert response.status_code == 200
    data = response.json()
    assert "euclidean" in data
    assert "geodesic" in data
    assert "padic_2" in data
    assert "padic_3" in data
    assert "weyl_distance" in data


def test_theta_coefficient():
    """Test theta series coefficient lookup"""
    response = client.get("/api/v1/e8-theta/coefficient/1")
    assert response.status_code == 200
    data = response.json()
    assert "n" in data
    assert "r_e8" in data
    assert data["n"] == 1
    assert data["r_e8"] >= 240


def test_qqf_link():
    """Test QQF linkage"""
    response = client.post(
        "/api/v1/e8-theta/qqf-link",
        json={
            "matrix": [
                [1.0, 0.0, 0.0, 0.0],
                [0.0, 1.0, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0]
            ]
        }
    )
    assert response.status_code == 200
    data = response.json()
    assert "determinant" in data
    assert "predicted_universality" in data
    assert "theta_growth_rate" in data
    assert "ramanujan_type" in data


def test_quorum_stability():
    """Test quorum stability prediction"""
    response = client.post(
        "/api/v1/e8-theta/stability",
        json={
            "voter_features": [
                [1.0, 2.0, 3.0, 4.0],
                [2.0, 3.0, 4.0, 5.0],
                [3.0, 4.0, 5.0, 6.0]
            ]
        }
    )
    assert response.status_code == 200
    data = response.json()
    assert "stability_score" in data
    assert "qqf_determinant" in data
    assert "theta_growth" in data
    assert "form_type" in data
    assert 0 <= data["stability_score"] <= 1


if __name__ == "__main__":
    pytest.main([__file__, "-v"])

