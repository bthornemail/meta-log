"""
Substrate API Service - FastAPI Application
Provides REST API for heavy substrate operations (hashing, compression)
Light operations are handled in R5RS Scheme.
"""

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List, Optional, Dict, Any
import hashlib
import base64

app = FastAPI(
    title="Substrate API",
    description="REST API for heavy substrate operations",
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


# Request/Response models
class HashRequest(BaseModel):
    data: str  # Base64 encoded bytes
    algorithm: str = "sha3-256"


class HashResponse(BaseModel):
    hash: str
    algorithm: str


class CompressRequest(BaseModel):
    data: str  # Base64 encoded bytes
    algorithm: str = "deflate"


class CompressResponse(BaseModel):
    compressed: str  # Base64 encoded
    original_size: int
    compressed_size: int
    ratio: float


@app.get("/api/v1/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "service": "substrate-api",
        "version": "1.0.0"
    }


@app.post("/api/v1/substrate/hash", response_model=HashResponse)
async def compute_hash(request: HashRequest):
    """
    Compute cryptographic hash of data.
    Used for content addressing when Scheme implementation is too slow.
    """
    try:
        data_bytes = base64.b64decode(request.data)
        
        if request.algorithm == "sha3-256":
            hash_obj = hashlib.sha3_256()
            hash_obj.update(data_bytes)
            hash_hex = hash_obj.hexdigest()
        elif request.algorithm == "sha3-512":
            hash_obj = hashlib.sha3_512()
            hash_obj.update(data_bytes)
            hash_hex = hash_obj.hexdigest()
        else:
            raise HTTPException(status_code=400, detail=f"Unsupported algorithm: {request.algorithm}")
        
        return {
            "hash": hash_hex,
            "algorithm": request.algorithm
        }
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.post("/api/v1/substrate/compress", response_model=CompressResponse)
async def compress_data(request: CompressRequest):
    """
    Compress binary data.
    Used for heavy compression operations.
    """
    try:
        import zlib
        
        data_bytes = base64.b64decode(request.data)
        original_size = len(data_bytes)
        
        if request.algorithm == "deflate":
            compressed = zlib.compress(data_bytes)
        else:
            raise HTTPException(status_code=400, detail=f"Unsupported algorithm: {request.algorithm}")
        
        compressed_size = len(compressed)
        ratio = compressed_size / original_size if original_size > 0 else 0.0
        
        return {
            "compressed": base64.b64encode(compressed).decode('utf-8'),
            "original_size": original_size,
            "compressed_size": compressed_size,
            "ratio": ratio
        }
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8001)

