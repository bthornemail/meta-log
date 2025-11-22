"""
E8 API Service - FastAPI Application
Provides REST API for E8 lattice and theta series operations
"""

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List, Optional, Dict, Any
import numpy as np

from e8_core import E8Lattice, E8Point
from e8_theta import E8ThetaSeries, E8VoterPredictor

app = FastAPI(
    title="E8 Lattice API",
    description="REST API for E8 exceptional Lie algebra operations",
    version="1.0.0"
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Initialize E8 structures
e8_lattice = E8Lattice()
e8_theta = E8ThetaSeries(max_norm=20)
e8_predictor = E8VoterPredictor(e8_theta)


# Request/Response models
class BIP32PathRequest(BaseModel):
    path: str


class E8PointResponse(BaseModel):
    coords: List[float]
    bip32_path: str
    depth: int
    norm_squared: float
    is_root: bool


class VerifyDelegationRequest(BaseModel):
    master_path: str
    delegate_path: str


class ShortestPathRequest(BaseModel):
    from_path: str
    to_path: str
    max_steps: Optional[int] = 48


class ShortestPathResponse(BaseModel):
    path: List[E8PointResponse]
    distance: float


class WeylOrbitRequest(BaseModel):
    path: str
    max_size: Optional[int] = 100


class WeylOrbitResponse(BaseModel):
    orbit: List[E8PointResponse]
    size: int


class DistanceMetricsResponse(BaseModel):
    euclidean: float
    geodesic: float
    padic_2: float
    padic_3: float
    weyl_distance: float


class QQFLinkRequest(BaseModel):
    matrix: List[List[float]]


class QuorumStabilityRequest(BaseModel):
    voter_features: List[List[float]]


class QuorumStabilityResponse(BaseModel):
    stability_score: float
    qqf_determinant: float
    theta_growth: float
    form_type: str


def e8_point_to_dict(point: E8Point) -> Dict[str, Any]:
    """Convert E8Point to dictionary"""
    return {
        "coords": point.coords.tolist(),
        "bip32_path": point.bip32_path,
        "depth": point.depth,
        "norm_squared": float(point.norm_squared()),
        "is_root": point.is_root()
    }


@app.get("/api/v1/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "e8_roots": len(e8_lattice.roots),
        "theta_coefficients": len(e8_theta.coefficients)
    }


@app.get("/api/v1/e8/point/{bip32_path:path}", response_model=E8PointResponse)
async def get_e8_point(bip32_path: str):
    """
    Get E8 point from BIP32 path
    
    Example: /api/v1/e8/point/m/44'/0'/0'/0/0
    """
    try:
        point = e8_lattice.bip32_to_e8(bip32_path)
        return e8_point_to_dict(point)
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.post("/api/v1/e8/verify", response_model=bool)
async def verify_frbac_delegation(request: VerifyDelegationRequest):
    """
    Verify FRBAC delegation using E8 automorphisms
    """
    try:
        master = e8_lattice.bip32_to_e8(request.master_path)
        delegate = e8_lattice.bip32_to_e8(request.delegate_path)
        return e8_lattice.verify_frbac_delegation(master, delegate)
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.post("/api/v1/e8/path", response_model=ShortestPathResponse)
async def shortest_path(request: ShortestPathRequest):
    """
    Find shortest path between two E8 points
    """
    try:
        start = e8_lattice.bip32_to_e8(request.from_path)
        end = e8_lattice.bip32_to_e8(request.to_path)
        path, distance = e8_lattice.shortest_path(start, end)
        
        return {
            "path": [e8_point_to_dict(p) for p in path],
            "distance": float(distance)
        }
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.post("/api/v1/e8/weyl-orbit", response_model=WeylOrbitResponse)
async def weyl_orbit(request: WeylOrbitRequest):
    """
    Compute Weyl group orbit of an E8 point
    """
    try:
        point = e8_lattice.bip32_to_e8(request.path)
        orbit = e8_lattice.weyl_orbit(point, max_size=request.max_size or 100)
        
        return {
            "orbit": [e8_point_to_dict(p) for p in orbit],
            "size": len(orbit)
        }
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.post("/api/v1/e8/distance", response_model=DistanceMetricsResponse)
async def distance_metrics(request: VerifyDelegationRequest):
    """
    Compute E8 distance metrics between two points
    """
    try:
        p1 = e8_lattice.bip32_to_e8(request.master_path)
        p2 = e8_lattice.bip32_to_e8(request.delegate_path)
        metrics = e8_lattice.distance_for_ml(p1, p2)
        
        return {
            "euclidean": float(metrics['euclidean']),
            "geodesic": float(metrics['geodesic']),
            "padic_2": float(metrics['padic_2']),
            "padic_3": float(metrics['padic_3']),
            "weyl_distance": float(metrics['weyl_distance'])
        }
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.get("/api/v1/e8-theta/coefficient/{n}")
async def get_theta_coefficient(n: int):
    """
    Get r_E8(n): theta series coefficient
    """
    try:
        coeff = e8_theta.coefficient(n)
        return {"n": n, "r_e8": coeff}
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.post("/api/v1/e8-theta/qqf-link")
async def link_to_qqf(request: QQFLinkRequest):
    """
    Link E8 theta series to quaternary quadratic form
    """
    try:
        matrix = np.array(request.matrix)
        analysis = e8_theta.link_to_qqf(matrix)
        
        return {
            "determinant": float(analysis['determinant']),
            "trace": float(analysis['trace']),
            "predicted_universality": analysis['predicted_universality'],
            "theta_growth_rate": float(analysis['theta_growth_rate']),
            "ramanujan_type": analysis['ramanujan_type']
        }
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.post("/api/v1/e8-theta/stability", response_model=QuorumStabilityResponse)
async def predict_quorum_stability(request: QuorumStabilityRequest):
    """
    Predict election quorum stability using theta series
    """
    try:
        features = np.array(request.voter_features)
        prediction = e8_theta.predict_quorum_stability(features)
        
        return {
            "stability_score": float(prediction['stability_score']),
            "qqf_determinant": float(prediction['qqf_determinant']),
            "theta_growth": float(prediction['theta_growth']),
            "form_type": prediction['form_type']
        }
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)

