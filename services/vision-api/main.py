#!/usr/bin/env python3
"""
Vision API Service - FastAPI service for heavy computer vision operations
Meta-Log Substrate System
Copyright (C) 2025 Meta-Log Research Group
"""

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List, Optional, Dict, Any
import numpy as np
import cv2
import base64
import io
from PIL import Image

app = FastAPI(title="Meta-Log Vision API", version="0.1.0")

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Request/Response Models
class ImageRequest(BaseModel):
    image_uri: str
    image_data: Optional[str] = None  # base64 encoded image

class FeatureExtractionRequest(BaseModel):
    image_uri: str
    image_data: Optional[str] = None
    params: Optional[Dict[str, Any]] = {}

class FeatureMatchRequest(BaseModel):
    features1_uri: str
    features2_uri: str
    features1_data: Optional[List[Dict]] = None
    features2_data: Optional[List[Dict]] = None
    threshold: float = 0.7

class FeaturePoint(BaseModel):
    x: float
    y: float
    scale: float
    orientation: float
    descriptor: List[float]

class FeatureExtractionResponse(BaseModel):
    features: List[FeaturePoint]
    count: int
    method: str

class FeatureMatch(BaseModel):
    feature1_idx: int
    feature2_idx: int
    distance: float
    confidence: float

class FeatureMatchResponse(BaseModel):
    matches: List[FeatureMatch]
    match_count: int

def load_image_from_data(image_data: str) -> np.ndarray:
    """Load image from base64 encoded data."""
    if image_data:
        image_bytes = base64.b64decode(image_data)
        image = Image.open(io.BytesIO(image_bytes))
        return np.array(image)
    return None

def load_image_from_uri(image_uri: str) -> Optional[np.ndarray]:
    """Load image from substrate URI.
    For now, returns None - would need substrate client."""
    # TODO: Implement substrate client to fetch image by URI
    return None

# SIFT Feature Extraction
@app.post("/vision/extract-sift", response_model=FeatureExtractionResponse)
async def extract_sift(request: FeatureExtractionRequest):
    """
    Extract SIFT features from image.
    """
    try:
        # Load image
        image = None
        if request.image_data:
            image = load_image_from_data(request.image_data)
        elif request.image_uri:
            image = load_image_from_uri(request.image_uri)
        
        if image is None:
            raise HTTPException(status_code=400, detail="No image data provided")
        
        # Convert to grayscale if needed
        if len(image.shape) == 3:
            gray = cv2.cvtColor(image, cv2.COLOR_RGB2GRAY)
        else:
            gray = image
        
        # Create SIFT detector
        sift = cv2.SIFT_create()
        
        # Detect keypoints and compute descriptors
        keypoints, descriptors = sift.detectAndCompute(gray, None)
        
        # Convert to response format
        features = []
        for kp, desc in zip(keypoints, descriptors):
            features.append(FeaturePoint(
                x=float(kp.pt[0]),
                y=float(kp.pt[1]),
                scale=float(kp.size),
                orientation=float(kp.angle),
                descriptor=desc.tolist()
            ))
        
        return FeatureExtractionResponse(
            features=features,
            count=len(features),
            method="sift"
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# ORB Feature Extraction
@app.post("/vision/extract-orb", response_model=FeatureExtractionResponse)
async def extract_orb(request: FeatureExtractionRequest):
    """
    Extract ORB features from image.
    """
    try:
        # Load image
        image = None
        if request.image_data:
            image = load_image_from_data(request.image_data)
        elif request.image_uri:
            image = load_image_from_uri(request.image_uri)
        
        if image is None:
            raise HTTPException(status_code=400, detail="No image data provided")
        
        # Convert to grayscale if needed
        if len(image.shape) == 3:
            gray = cv2.cvtColor(image, cv2.COLOR_RGB2GRAY)
        else:
            gray = image
        
        # Create ORB detector
        orb = cv2.ORB_create()
        
        # Detect keypoints and compute descriptors
        keypoints, descriptors = orb.detectAndCompute(gray, None)
        
        # Convert to response format
        features = []
        for kp, desc in zip(keypoints, descriptors):
            features.append(FeaturePoint(
                x=float(kp.pt[0]),
                y=float(kp.pt[1]),
                scale=float(kp.size),
                orientation=float(kp.angle),
                descriptor=desc.tolist()
            ))
        
        return FeatureExtractionResponse(
            features=features,
            count=len(features),
            method="orb"
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# Feature Matching
@app.post("/vision/match-features", response_model=FeatureMatchResponse)
async def match_features(request: FeatureMatchRequest):
    """
    Match features between two feature sets.
    """
    try:
        # Load feature descriptors
        features1 = request.features1_data or []
        features2 = request.features2_data or []
        
        if not features1 or not features2:
            raise HTTPException(status_code=400, detail="Feature data required")
        
        # Extract descriptors
        desc1 = np.array([f["descriptor"] for f in features1], dtype=np.float32)
        desc2 = np.array([f["descriptor"] for f in features2], dtype=np.float32)
        
        # Use FLANN matcher for SIFT, brute force for ORB
        if len(desc1) > 0 and len(desc2) > 0:
            # Try FLANN first (for SIFT)
            try:
                FLANN_INDEX_KDTREE = 1
                index_params = dict(algorithm=FLANN_INDEX_KDTREE, trees=5)
                search_params = dict(checks=50)
                flann = cv2.FlannBasedMatcher(index_params, search_params)
                matches = flann.knnMatch(desc1, desc2, k=2)
            except:
                # Fallback to brute force
                bf = cv2.BFMatcher(cv2.NORM_HAMMING, crossCheck=True)
                matches = bf.match(desc1, desc2)
                matches = [[m] for m in matches]
            
            # Apply ratio test and threshold
            good_matches = []
            for match_group in matches:
                if len(match_group) == 2:
                    m, n = match_group
                    if m.distance < request.threshold * n.distance:
                        good_matches.append(FeatureMatch(
                            feature1_idx=int(m.queryIdx),
                            feature2_idx=int(m.trainIdx),
                            distance=float(m.distance),
                            confidence=1.0 - (m.distance / (n.distance + 1e-6))
                        ))
                elif len(match_group) == 1:
                    m = match_group[0]
                    if m.distance < request.threshold * 100:  # Threshold for single match
                        good_matches.append(FeatureMatch(
                            feature1_idx=int(m.queryIdx),
                            feature2_idx=int(m.trainIdx),
                            distance=float(m.distance),
                            confidence=1.0 - (m.distance / 100.0)
                        ))
        else:
            good_matches = []
        
        return FeatureMatchResponse(
            matches=good_matches,
            match_count=len(good_matches)
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# Health check
@app.get("/health")
async def health():
    return {"status": "ok", "service": "vision-api"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8002)

