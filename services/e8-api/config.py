"""
E8 API Service Configuration
"""

import os
from typing import Optional

class Config:
    """Application configuration"""
    
    # Server settings
    HOST: str = os.getenv("E8_API_HOST", "0.0.0.0")
    PORT: int = int(os.getenv("E8_API_PORT", "8000"))
    
    # E8 settings
    E8_GRAPH_PATH: Optional[str] = os.getenv("E8_GRAPH_PATH")
    E8_MAX_NORM: int = int(os.getenv("E8_MAX_NORM", "20"))
    
    # Performance limits
    WEYL_ORBIT_MAX_SIZE: int = int(os.getenv("WEYL_ORBIT_MAX_SIZE", "100"))
    SHORTEST_PATH_MAX_STEPS: int = int(os.getenv("SHORTEST_PATH_MAX_STEPS", "48"))
    
    # CORS
    CORS_ORIGINS: list = os.getenv("CORS_ORIGINS", "*").split(",")

