"""
E8 API Integration Tests
Tests for Python FastAPI service
"""

import pytest
import requests
import numpy as np
from httpx import ASGITransport, AsyncClient

from app import app

# Use httpx AsyncClient for starlette 0.27.0 compatibility
@pytest.fixture
def client():
    """Create async test client"""
    transport = ASGITransport(app=app)
    return AsyncClient(transport=transport, base_url="http://test")


@pytest.mark.asyncio
async def test_health_check(client):
    """Test health check endpoint"""
    response = await client.get("/api/v1/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "healthy"
    assert "e8_roots" in data
    assert "theta_coefficients" in data


@pytest.mark.asyncio
async def test_get_e8_point(client):
    """Test getting E8 point from BIP32 path"""
    response = await client.get("/api/v1/e8/point/m/44'/0'/0'/0/0")
    assert response.status_code == 200
    data = response.json()
    assert "coords" in data
    assert len(data["coords"]) == 8
    assert data["bip32_path"] == "m/44'/0'/0'/0/0"
    assert "norm_squared" in data
    assert "is_root" in data


@pytest.mark.asyncio
async def test_get_e8_point_invalid_path(client):
    """Test getting E8 point with invalid path"""
    response = await client.get("/api/v1/e8/point/invalid/path")
    assert response.status_code == 400


@pytest.mark.asyncio
async def test_verify_frbac_delegation(client):
    """Test FRBAC delegation verification"""
    response = await client.post(
        "/api/v1/e8/verify",
        json={
            "master_path": "m/44'/0'/0'",
            "delegate_path": "m/44'/0'/0'/0/0"
        }
    )
    assert response.status_code == 200
    assert isinstance(response.json(), bool)


@pytest.mark.asyncio
async def test_verify_frbac_delegation_invalid(client):
    """Test FRBAC delegation with invalid paths"""
    response = await client.post(
        "/api/v1/e8/verify",
        json={
            "master_path": "invalid",
            "delegate_path": "invalid"
        }
    )
    assert response.status_code == 400


@pytest.mark.asyncio
async def test_shortest_path(client):
    """Test shortest path computation"""
    response = await client.post(
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


@pytest.mark.asyncio
async def test_shortest_path_invalid(client):
    """Test shortest path with invalid paths"""
    response = await client.post(
        "/api/v1/e8/path",
        json={
            "from_path": "invalid",
            "to_path": "invalid"
        }
    )
    assert response.status_code == 400


@pytest.mark.asyncio
async def test_weyl_orbit(client):
    """Test Weyl orbit computation"""
    response = await client.post(
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


@pytest.mark.asyncio
async def test_weyl_orbit_invalid(client):
    """Test Weyl orbit with invalid path"""
    response = await client.post(
        "/api/v1/e8/weyl-orbit",
        json={
            "path": "invalid",
            "max_size": 10
        }
    )
    assert response.status_code == 400


@pytest.mark.asyncio
async def test_distance_metrics(client):
    """Test distance metrics computation"""
    response = await client.post(
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


@pytest.mark.asyncio
async def test_distance_metrics_invalid(client):
    """Test distance metrics with invalid paths"""
    response = await client.post(
        "/api/v1/e8/distance",
        json={
            "master_path": "invalid",
            "delegate_path": "invalid"
        }
    )
    assert response.status_code == 400


@pytest.mark.asyncio
async def test_theta_coefficient(client):
    """Test theta series coefficient lookup"""
    response = await client.get("/api/v1/e8-theta/coefficient/1")
    assert response.status_code == 200
    data = response.json()
    assert "n" in data
    assert "r_e8" in data
    assert data["n"] == 1
    assert data["r_e8"] >= 240


@pytest.mark.asyncio
async def test_theta_coefficient_invalid(client):
    """Test theta coefficient with invalid n"""
    response = await client.get("/api/v1/e8-theta/coefficient/-1")
    # Should handle invalid input
    assert response.status_code in [200, 400, 404]


@pytest.mark.asyncio
async def test_qqf_link(client):
    """Test QQF linkage"""
    response = await client.post(
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


@pytest.mark.asyncio
async def test_qqf_link_invalid_matrix(client):
    """Test QQF link with invalid matrix"""
    response = await client.post(
        "/api/v1/e8-theta/qqf-link",
        json={
            "matrix": [[1.0]]  # Invalid size
        }
    )
    # Should handle invalid input
    assert response.status_code in [200, 400]


@pytest.mark.asyncio
async def test_quorum_stability(client):
    """Test quorum stability prediction"""
    response = await client.post(
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


@pytest.mark.asyncio
async def test_quorum_stability_invalid(client):
    """Test quorum stability with invalid features"""
    response = await client.post(
        "/api/v1/e8-theta/stability",
        json={
            "voter_features": []  # Empty list
        }
    )
    # Should handle invalid input
    assert response.status_code in [200, 400]


if __name__ == "__main__":
    pytest.main([__file__, "-v"])

