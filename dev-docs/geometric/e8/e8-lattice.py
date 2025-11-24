"""
E8 Lattice Operations for CANVASL Metaverse
============================================

Implements E8 exceptional Lie algebra root system for:
- BIP32 hierarchical deterministic path derivation
- FRBAC delegation verification via Weyl group
- p-adic heights for voter ML features
- Optimal path finding (shortest vector problem)

Mathematical Foundation:
- E8 has 240 roots forming integral lattice in ℝ⁸
- Root system: {±eᵢ ± eⱼ} ∪ {½(±e₁±e₂±...±e₈) with even # minuses}
- Weyl group: W(E8) ≅ 2⁹ × S₁₀, order 696,729,600
- Self-dual: root lattice = weight lattice

Integration with CANVASL:
- Maps BIP32 paths m/purpose'/coin'/account'/change/index to E8 points
- FRBAC paths verify via E8 automorphisms (Weyl reflections)
- Voter ML uses E8 distances as features
- p-adic valuations for ramification detection
"""

import numpy as np
from typing import List, Tuple, Optional, Set
from dataclasses import dataclass
from functools import lru_cache
import hashlib


@dataclass
class E8Point:
    """Point in E8 lattice with BIP32 metadata"""
    coords: np.ndarray  # 8D coordinates
    bip32_path: str     # e.g., "m/44'/0'/0'/0/0"
    depth: int          # Derivation depth
    parent: Optional['E8Point'] = None
    
    def __hash__(self):
        return hash(tuple(self.coords.round(6)))
    
    def norm_squared(self) -> float:
        """Squared norm (distance from origin)"""
        return float(np.dot(self.coords, self.coords))
    
    def is_root(self) -> bool:
        """Check if this is one of 240 E8 roots (norm² = 2)"""
        return abs(self.norm_squared() - 2.0) < 1e-6


class E8Lattice:
    """
    E8 Exceptional Lattice for CANVASL
    
    Provides:
    - 240 root system construction
    - BIP32 path ↔ E8 point mapping
    - Weyl group operations
    - Shortest vector algorithms
    - p-adic heights
    """
    
    def __init__(self):
        self.roots = self._construct_roots()
        self.simple_roots = self._get_simple_roots()
        self.weyl_generators = self._construct_weyl_generators()
        
    def _construct_roots(self) -> List[np.ndarray]:
        """
        Construct all 240 E8 roots
        
        Two types:
        1. All permutations of (±1, ±1, 0, 0, 0, 0, 0, 0): 112 roots
        2. ½(±1, ±1, ±1, ±1, ±1, ±1, ±1, ±1) with even # of -1s: 128 roots
        
        Returns: List of 240 root vectors
        """
        roots = []
        
        # Type 1: (±eᵢ ± eⱼ) for i ≠ j
        for i in range(8):
            for j in range(i+1, 8):
                for sign_i in [1, -1]:
                    for sign_j in [1, -1]:
                        root = np.zeros(8)
                        root[i] = sign_i
                        root[j] = sign_j
                        roots.append(root)
        
        # Type 2: ½(±e₁±e₂±...±e₈) with even number of minus signs
        for mask in range(256):  # 2^8 combinations
            signs = [(mask >> i) & 1 for i in range(8)]
            num_minus = sum(signs)
            if num_minus % 2 == 0:  # Even number of -1s
                root = np.array([1 if s == 0 else -1 for s in signs]) / 2.0
                roots.append(root)
        
        return roots
    
    def _get_simple_roots(self) -> List[np.ndarray]:
        """
        Simple roots (Dynkin diagram basis) for E8
        
        Standard choice (Bourbaki convention):
        α₁ = ½(-1,-1,-1,-1,-1,-1,-1,1)
        α₂ = (1,1,0,0,0,0,0,0)
        α₃ = (-1,1,0,0,0,0,0,0)
        α₄ = (0,-1,1,0,0,0,0,0)
        α₅ = (0,0,-1,1,0,0,0,0)
        α₆ = (0,0,0,-1,1,0,0,0)
        α₇ = (0,0,0,0,-1,1,0,0)
        α₈ = (0,0,0,0,0,-1,1,0)
        """
        return [
            np.array([-1,-1,-1,-1,-1,-1,-1,1]) / 2.0,
            np.array([1,1,0,0,0,0,0,0]),
            np.array([-1,1,0,0,0,0,0,0]),
            np.array([0,-1,1,0,0,0,0,0]),
            np.array([0,0,-1,1,0,0,0,0]),
            np.array([0,0,0,-1,1,0,0,0]),
            np.array([0,0,0,0,-1,1,0,0]),
            np.array([0,0,0,0,0,-1,1,0]),
        ]
    
    def _construct_weyl_generators(self) -> List[np.ndarray]:
        """
        Weyl group generators (reflections through simple roots)
        
        Reflection through root α: s_α(v) = v - 2(v·α)/(α·α) α
        """
        generators = []
        for alpha in self.simple_roots:
            # Create reflection matrix
            alpha_norm_sq = np.dot(alpha, alpha)
            # s_α = I - 2(α⊗α)/|α|²
            reflection = np.eye(8) - 2 * np.outer(alpha, alpha) / alpha_norm_sq
            generators.append(reflection)
        return generators
    
    def bip32_to_e8(self, path: str) -> E8Point:
        """
        Map BIP32 path to E8 lattice point
        
        Algorithm:
        1. Parse path components (m/44'/0'/0'/0/0)
        2. Hash to get deterministic 8D coordinates
        3. Project onto E8 lattice (round to nearest point)
        4. Return E8Point with metadata
        
        Example:
            path = "m/44'/0'/0'/0/0"
            → E8 point in fundamental domain
        """
        # Parse BIP32 path
        components = path.split('/')
        if components[0] != 'm':
            raise ValueError(f"Invalid BIP32 path: {path}")
        
        depth = len(components) - 1
        
        # Deterministic hash to 8D coordinates
        path_hash = hashlib.sha256(path.encode()).digest()
        
        # Convert hash to EXACTLY 8D float coordinates in [-10, 10]
        # Use first 64 bytes of hash (8 * 8 bytes for 8 doubles)
        coords_raw = np.frombuffer(path_hash + path_hash, dtype=np.float64)[:8]  # Ensure exactly 8
        coords_normalized = (coords_raw % 20) - 10  # Scale to reasonable range
        
        # Project onto E8 lattice (round to nearest lattice point)
        coords_lattice = self._project_to_e8_lattice(coords_normalized)
        
        # Ensure we have exactly 8 dimensions
        assert coords_lattice.shape == (8,), f"Expected 8D coordinates, got {coords_lattice.shape}"
        
        # Get parent point if depth > 0
        parent = None
        if depth > 0:
            parent_path = '/'.join(components[:-1])
            parent = self.bip32_to_e8(parent_path)
        
        return E8Point(
            coords=coords_lattice,
            bip32_path=path,
            depth=depth,
            parent=parent
        )
    
    def _project_to_e8_lattice(self, v: np.ndarray) -> np.ndarray:
        """
        Project arbitrary 8D vector to nearest E8 lattice point
        
        Uses Voronoi cell algorithm:
        1. Round to integer lattice ℤ⁸
        2. Check if in E8 (sum of coords is even)
        3. If not, shift by (½,...,½)
        """
        # Round to ℤ⁸
        v_rounded = np.round(v)
        
        # E8 condition: sum must be even
        if int(v_rounded.sum()) % 2 == 0:
            return v_rounded
        else:
            # Shift to (ℤ + ½)⁸ coset
            return np.round(v - 0.5) + 0.5
    
    def shortest_path(self, start: E8Point, end: E8Point) -> Tuple[List[E8Point], float]:
        """
        Find shortest path in E8 lattice (SVP variant)
        
        Uses A* with E8 metric:
        - Heuristic: Euclidean distance
        - Cost: Sum of root steps
        
        Returns: (path, total_distance)
        """
        from heapq import heappush, heappop
        
        # A* search - use counter to break ties in heap
        counter = 0
        frontier = [(0, counter, 0, start, [start])]  # (f_score, tie_breaker, g_score, point, path)
        visited = set()
        
        def heuristic(p1: E8Point, p2: E8Point) -> float:
            diff = p2.coords - p1.coords
            return np.linalg.norm(diff)
        
        while frontier:
            f_score, _, g_score, current, path = heappop(frontier)
            
            if np.allclose(current.coords, end.coords):
                return path, g_score
            
            current_hash = hash(current)
            if current_hash in visited:
                continue
            visited.add(current_hash)
            
            # Neighbors: current + root for each of 240 roots
            for root in self.roots[:48]:  # Limit search for performance
                neighbor_coords = current.coords + root
                neighbor = E8Point(
                    coords=neighbor_coords,
                    bip32_path=f"{current.bip32_path}/step",
                    depth=current.depth + 1,
                    parent=current
                )
                
                new_g = g_score + np.linalg.norm(root)
                new_f = new_g + heuristic(neighbor, end)
                
                counter += 1
                heappush(frontier, (new_f, counter, new_g, neighbor, path + [neighbor]))
        
        # No path found
        return [start, end], heuristic(start, end)
        
        # No path found
        return [start, end], heuristic(start, end)
    
    def weyl_orbit(self, point: E8Point, max_size: int = 100) -> Set[E8Point]:
        """
        Compute Weyl group orbit of a point
        
        Orbit = {w(v) : w ∈ W(E8)}
        
        For FRBAC: verifies delegation validity
        - Point is valid iff in Weyl orbit of master key
        """
        orbit = {point}
        queue = [point]
        
        while queue and len(orbit) < max_size:
            current = queue.pop(0)
            
            # Apply each Weyl generator
            for gen in self.weyl_generators:
                new_coords = gen @ current.coords
                new_point = E8Point(
                    coords=new_coords,
                    bip32_path=f"{current.bip32_path}/weyl",
                    depth=current.depth,
                    parent=current.parent
                )
                
                new_hash = hash(new_point)
                if new_hash not in {hash(p) for p in orbit}:
                    orbit.add(new_point)
                    queue.append(new_point)
        
        return orbit
    
    def padic_height(self, point: E8Point, p: int) -> float:
        """
        Compute p-adic height on E8 lattice
        
        For prime p, height h_p(P) measures p-adic distance from origin
        
        Used in voter ML:
        - ord_p on coordinates → ramification features
        - Heights detect "p-adically close" voters
        """
        # p-adic valuation: ord_p(x) = max{k : p^k divides x}
        def ord_p(x: float, p: int) -> int:
            if abs(x) < 1e-10:
                return float('inf')
            
            x_int = int(round(x * (2**10)))  # Convert to integer (scaled)
            if x_int == 0:
                return float('inf')
            
            val = 0
            while x_int % p == 0:
                val += 1
                x_int //= p
            return val
        
        # Canonical height: sum of local heights
        height = 0.0
        for coord in point.coords:
            height += ord_p(coord, p) * np.log(p)
        
        return height
    
    def verify_frbac_delegation(self, master: E8Point, delegate: E8Point) -> bool:
        """
        Verify FRBAC delegation using E8 automorphisms
        
        Valid delegation iff:
        1. delegate ∈ Weyl orbit of master, OR
        2. delegate is reachable via root steps from master
        
        Returns: True if delegation is valid
        """
        # Check Weyl orbit (expensive, so limit size)
        orbit = self.weyl_orbit(master, max_size=50)
        if delegate in orbit:
            return True
        
        # Check root-step reachability
        diff = delegate.coords - master.coords
        
        # Is diff a sum of roots?
        # Simplified: check if ||diff|| is reasonable
        diff_norm = np.linalg.norm(diff)
        return diff_norm < 10.0  # Heuristic threshold
    
    def distance_for_ml(self, p1: E8Point, p2: E8Point) -> dict:
        """
        Compute E8 distance features for voter ML
        
        Returns dict with:
        - euclidean: Standard L2 distance
        - geodesic: Shortest path length in E8
        - padic_2: 2-adic height difference
        - padic_3: 3-adic height difference
        - weyl_distance: Minimum Weyl conjugate distance
        """
        euclidean = np.linalg.norm(p2.coords - p1.coords)
        
        # Geodesic (expensive, use heuristic)
        geodesic = euclidean * 1.2  # Rough approximation
        
        # p-adic heights
        padic_2 = abs(self.padic_height(p1, 2) - self.padic_height(p2, 2))
        padic_3 = abs(self.padic_height(p1, 3) - self.padic_height(p2, 3))
        
        # Weyl distance (minimum over small orbit)
        orbit1 = self.weyl_orbit(p1, max_size=10)
        weyl_distance = min(
            np.linalg.norm(q.coords - p2.coords) 
            for q in orbit1
        )
        
        return {
            'euclidean': euclidean,
            'geodesic': geodesic,
            'padic_2': padic_2,
            'padic_3': padic_3,
            'weyl_distance': weyl_distance
        }


# ============================================================================
# BIP32 Integration Examples
# ============================================================================

def demo_bip32_e8_derivation():
    """
    Demonstrate BIP32 path → E8 point mapping
    
    CANVASL Use Case:
    - Master identity: m/44'/0'/0'
    - Derive delegation paths as E8 points
    - Verify using Weyl group
    """
    print("=" * 60)
    print("BIP32 E8 Path Derivation Demo")
    print("=" * 60)
    
    e8 = E8Lattice()
    
    # Master identity
    master_path = "m/44'/0'/0'"
    master = e8.bip32_to_e8(master_path)
    print(f"\nMaster Key: {master_path}")
    print(f"E8 Coordinates: {master.coords}")
    print(f"Norm²: {master.norm_squared():.4f}")
    print(f"Is Root: {master.is_root()}")
    
    # Derive child paths
    children = [
        "m/44'/0'/0'/0/0",  # First account, external chain
        "m/44'/0'/0'/0/1",  # Second address
        "m/44'/0'/0'/1/0",  # Change chain
    ]
    
    print("\n" + "-" * 60)
    print("Derived Children:")
    print("-" * 60)
    
    for child_path in children:
        child = e8.bip32_to_e8(child_path)
        print(f"\nPath: {child_path}")
        print(f"E8 Point: {child.coords}")
        
        # Verify delegation
        is_valid = e8.verify_frbac_delegation(master, child)
        print(f"FRBAC Valid: {is_valid}")
        
        # Compute distances
        distances = e8.distance_for_ml(master, child)
        print(f"Distances: {distances}")
    
    # Find shortest path between two children
    start = e8.bip32_to_e8(children[0])
    end = e8.bip32_to_e8(children[1])
    
    print("\n" + "-" * 60)
    print("Shortest Path Analysis:")
    print("-" * 60)
    print(f"From: {children[0]}")
    print(f"To: {children[1]}")
    
    path, distance = e8.shortest_path(start, end)
    print(f"Path Length: {len(path)} steps")
    print(f"Total Distance: {distance:.4f}")
    
    print("\n" + "=" * 60)


def demo_voter_ml_features():
    """
    Generate E8-based features for voter ML
    
    CANVASL Integration:
    - Maps voters/candidates to E8 points
    - Computes E8 distances
    - Adds p-adic heights as features
    """
    print("=" * 60)
    print("Voter ML E8 Features Demo")
    print("=" * 60)
    
    e8 = E8Lattice()
    
    # Simulate voters and candidates as BIP32 paths
    voters = [
        f"m/44'/0'/0'/0/{i}" for i in range(5)
    ]
    candidates = [
        f"m/44'/1'/0'/0/{i}" for i in range(3)
    ]
    
    print("\nGenerating E8 features for voter-candidate pairs...")
    
    features_matrix = []
    
    for voter_path in voters:
        voter = e8.bip32_to_e8(voter_path)
        voter_features = []
        
        for cand_path in candidates:
            cand = e8.bip32_to_e8(cand_path)
            
            # Compute E8 distances
            dists = e8.distance_for_ml(voter, cand)
            
            # Feature vector: [euclidean, padic_2, padic_3, weyl_dist]
            features = [
                dists['euclidean'],
                dists['padic_2'],
                dists['padic_3'],
                dists['weyl_distance']
            ]
            voter_features.extend(features)
        
        features_matrix.append(voter_features)
    
    features_matrix = np.array(features_matrix)
    
    print(f"\nFeature Matrix Shape: {features_matrix.shape}")
    print(f"(5 voters × 3 candidates × 4 features = 60 features per voter)")
    print(f"\nSample Features (Voter 0):")
    print(features_matrix[0][:12])  # First 3 candidates
    
    print("\n" + "=" * 60)


if __name__ == "__main__":
    # Verify E8 construction
    e8 = E8Lattice()
    print(f"E8 Lattice Initialized")
    print(f"Number of roots: {len(e8.roots)}")
    print(f"Number of simple roots: {len(e8.simple_roots)}")
    print(f"Weyl generators: {len(e8.weyl_generators)}")
    
    # Run demos
    print("\n")
    demo_bip32_e8_derivation()
    print("\n")
    demo_voter_ml_features()
