"""
Combined E8 Example: Enhanced A11 Voter Prediction
===================================================

This example demonstrates the full integration of:
1. E8 lattice for BIP32 identity mapping
2. E8 theta series for QQF discriminant analysis
3. Enhanced ML voting prediction with E8 features

Use Case: A11 swarm election with exceptional geometry
"""

import numpy as np
from typing import List, Tuple
import sys
import os

# Add current directory to path
sys.path.insert(0, '/home/claude')

# Import by loading the modules directly
import importlib.util

def load_module(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

e8_lattice_mod = load_module("e8_lattice", "/home/claude/e8-lattice.py")
e8_theta_mod = load_module("e8_theta", "/home/claude/e8-theta-series.py")

E8Lattice = e8_lattice_mod.E8Lattice
E8Point = e8_lattice_mod.E8Point
E8ThetaSeries = e8_theta_mod.E8ThetaSeries
E8VoterPredictor = e8_theta_mod.E8VoterPredictor


class E8EnhancedSwarmElection:
    """
    Complete A11 election simulation with E8 exceptional geometry
    
    Combines:
    - BIP32 identities mapped to E8 points
    - E8 distance features for voter-candidate affinity
    - Theta series for quorum stability prediction
    - p-adic heights for partition detection
    """
    
    def __init__(self, n_voters: int = 10, n_candidates: int = 3):
        self.n_voters = n_voters
        self.n_candidates = n_candidates
        
        # Initialize E8 structures
        self.e8 = E8Lattice()
        self.theta = E8ThetaSeries(max_norm=10)
        self.theta_predictor = E8VoterPredictor(self.theta)
        
        print("=" * 70)
        print("E8-Enhanced Swarm Election Initialized")
        print("=" * 70)
        print(f"Voters: {n_voters}")
        print(f"Candidates: {n_candidates}")
        print(f"E8 Roots: {len(self.e8.roots)}")
        print(f"Theta coefficients computed: {len(self.theta.coefficients)}")
        print()
    
    def create_swarm_identities(self) -> Tuple[List[E8Point], List[E8Point]]:
        """
        Map voters and candidates to E8 lattice points via BIP32
        
        Returns:
            (voter_points, candidate_points)
        """
        # Voters use path: m/44'/swarm'/0'/voter/{i}
        voter_paths = [f"m/44'/0'/0'/0/{i}" for i in range(self.n_voters)]
        voter_points = [self.e8.bip32_to_e8(path) for path in voter_paths]
        
        # Candidates use path: m/44'/swarm'/1'/candidate/{j}
        candidate_paths = [f"m/44'/1'/0'/0/{j}" for j in range(self.n_candidates)]
        candidate_points = [self.e8.bip32_to_e8(path) for path in candidate_paths]
        
        print("Swarm Identities Generated")
        print("-" * 70)
        
        for i, (path, point) in enumerate(zip(voter_paths[:3], voter_points[:3])):
            print(f"Voter {i}: {path}")
            print(f"  E8 coords: {point.coords[:4]}...")  # Show first 4 dims
            print(f"  Norm²: {point.norm_squared():.2f}")
        
        print()
        for j, (path, point) in enumerate(zip(candidate_paths, candidate_points)):
            print(f"Candidate {j}: {path}")
            print(f"  E8 coords: {point.coords[:4]}...")
            print(f"  Norm²: {point.norm_squared():.2f}")
        
        print()
        return voter_points, candidate_points
    
    def compute_e8_features(
        self,
        voter_points: List[E8Point],
        candidate_points: List[E8Point]
    ) -> np.ndarray:
        """
        Compute E8-based features for each voter-candidate pair
        
        Features per pair:
        - Euclidean distance in E8
        - 2-adic height difference
        - 3-adic height difference  
        - Weyl orbit distance (minimum conjugate)
        
        Returns:
            (n_voters, n_candidates * 4) feature matrix
        """
        features = []
        
        for voter in voter_points:
            voter_features = []
            
            for candidate in candidate_points:
                # Get full E8 distance metrics
                dists = self.e8.distance_for_ml(voter, candidate)
                
                # Extract key features
                voter_features.extend([
                    dists['euclidean'],
                    dists['padic_2'],
                    dists['padic_3'],
                    dists['weyl_distance']
                ])
            
            features.append(voter_features)
        
        features_matrix = np.array(features)
        
        print("E8 Features Computed")
        print("-" * 70)
        print(f"Feature matrix shape: {features_matrix.shape}")
        print(f"({self.n_voters} voters × {self.n_candidates} candidates × 4 features)")
        print(f"\nSample features (Voter 0, Candidate 0):")
        print(f"  Euclidean: {features_matrix[0,0]:.4f}")
        print(f"  p-adic (2): {features_matrix[0,1]:.4f}")
        print(f"  p-adic (3): {features_matrix[0,2]:.4f}")
        print(f"  Weyl dist: {features_matrix[0,3]:.4f}")
        print()
        
        return features_matrix
    
    def analyze_quorum_stability(self, features: np.ndarray) -> dict:
        """
        Use E8 theta series to predict quorum stability
        
        Args:
            features: E8 feature matrix
        
        Returns:
            Stability analysis dict
        """
        # Use theta series predictor
        prediction = self.theta.predict_quorum_stability(features)
        
        print("Quorum Stability Analysis (E8 Theta Series)")
        print("-" * 70)
        print(f"Stability Score: {prediction['stability_score']:.4f}")
        print(f"QQF Determinant: {prediction['qqf_determinant']:.4f}")
        print(f"Theta Growth Rate: {prediction['theta_growth']:.4f}")
        print(f"Form Type: {prediction['form_type']}")
        
        # Classify stability
        if prediction['stability_score'] > 0.7:
            status = "STABLE - Definite form, low partition risk"
        elif prediction['stability_score'] > 0.4:
            status = "MODERATE - Monitor for β₀ increases"
        else:
            status = "UNSTABLE - Indefinite form, high partition risk"
        
        print(f"Status: {status}")
        print()
        
        return prediction
    
    def simulate_vote(
        self,
        features: np.ndarray,
        candidate_points: List[E8Point]
    ) -> dict:
        """
        Simulate voting based on E8 distances
        
        Voting rule: Each voter chooses nearest candidate in E8 metric
        
        Returns:
            Election results dict
        """
        votes = {i: 0 for i in range(self.n_candidates)}
        voter_choices = []
        
        # Each voter picks nearest candidate
        for v_idx in range(self.n_voters):
            # Get distances to all candidates (every 4th feature, starting at 0)
            euclidean_dists = [
                features[v_idx, c*4]  # Euclidean is first feature
                for c in range(self.n_candidates)
            ]
            
            # Choose nearest
            chosen = np.argmin(euclidean_dists)
            votes[chosen] += 1
            voter_choices.append(chosen)
        
        # Determine winner
        winner = max(votes, key=votes.get)
        
        print("Election Results (E8 Nearest Neighbor)")
        print("-" * 70)
        for cand_idx, count in votes.items():
            percentage = 100 * count / self.n_voters
            winner_mark = " ← WINNER" if cand_idx == winner else ""
            print(f"Candidate {cand_idx}: {count} votes ({percentage:.1f}%){winner_mark}")
        
        print()
        
        return {
            'votes': votes,
            'winner': winner,
            'choices': voter_choices
        }
    
    def verify_frbac_delegations(
        self,
        voter_points: List[E8Point],
        candidate_points: List[E8Point]
    ):
        """
        Verify FRBAC delegations using E8 Weyl group
        
        Checks if voters can delegate to candidates via E8 automorphisms
        """
        print("FRBAC Delegation Verification (E8 Weyl Group)")
        print("-" * 70)
        
        for v_idx in range(min(3, self.n_voters)):  # Check first 3 voters
            voter = voter_points[v_idx]
            
            for c_idx in range(self.n_candidates):
                candidate = candidate_points[c_idx]
                
                # Verify via E8 Weyl group
                is_valid = self.e8.verify_frbac_delegation(voter, candidate)
                
                status = "✓ VALID" if is_valid else "✗ INVALID"
                print(f"Voter {v_idx} → Candidate {c_idx}: {status}")
        
        print()
    
    def run_full_election(self):
        """
        Execute complete E8-enhanced election
        """
        print("\n" + "=" * 70)
        print("Running E8-Enhanced A11 Election")
        print("=" * 70 + "\n")
        
        # Step 1: Create identities
        voter_points, candidate_points = self.create_swarm_identities()
        
        # Step 2: Compute E8 features
        features = self.compute_e8_features(voter_points, candidate_points)
        
        # Step 3: Analyze stability
        stability = self.analyze_quorum_stability(features)
        
        # Step 4: Simulate vote
        results = self.simulate_vote(features, candidate_points)
        
        # Step 5: Verify FRBAC
        self.verify_frbac_delegations(voter_points, candidate_points)
        
        # Summary
        print("=" * 70)
        print("Election Summary")
        print("=" * 70)
        print(f"Winner: Candidate {results['winner']}")
        print(f"Stability: {stability['stability_score']:.4f}")
        print(f"Form Type: {stability['form_type']}")
        print(f"Quorum Health: {'GOOD' if stability['stability_score'] > 0.5 else 'AT RISK'}")
        print("=" * 70)
        
        return results, stability


def demo_e8_theta_coefficients():
    """
    Demonstrate E8 theta series classical values
    """
    print("\n" + "=" * 70)
    print("E8 Theta Series Classical Values")
    print("=" * 70 + "\n")
    
    theta = E8ThetaSeries(max_norm=10)
    
    # Classical E8 theta series values
    classical_values = [
        (0, 1),
        (1, 240),
        (2, 2160),
        (3, 6720),
        (4, 17520),
    ]
    
    print("n    r_E8(n)  Expected  Match")
    print("-" * 70)
    
    for n, expected in classical_values:
        computed = theta.coefficient(n)
        match = "✓" if computed == expected else "✗"
        print(f"{n:2d}   {computed:6d}   {expected:6d}    {match}")
    
    print()


def demo_quaternion_algebra_link():
    """
    Demonstrate link between quaternary forms and E8
    """
    print("=" * 70)
    print("Quaternary Quadratic Forms ↔ E8 Theta Series")
    print("=" * 70 + "\n")
    
    theta = E8ThetaSeries(max_norm=10)
    
    # Different QQF types
    forms = {
        "Sum of 4 squares": np.eye(4),
        "Ramanujan x²+y²+10z²+10w²": np.diag([1, 1, 10, 10]),
        "Indefinite (3,1) signature": np.diag([1, 1, 1, -1]),
    }
    
    for name, matrix in forms.items():
        print(f"{name}:")
        analysis = theta.link_to_qqf(matrix)
        print(f"  Determinant: {analysis['determinant']:.4f}")
        print(f"  Universal: {analysis['predicted_universality']}")
        print(f"  Type: {analysis['ramanujan_type']}")
        print()


if __name__ == "__main__":
    # Demo 1: E8 theta series classical values
    demo_e8_theta_coefficients()
    
    # Demo 2: QQF linkage
    demo_quaternion_algebra_link()
    
    # Demo 3: Full election simulation
    election = E8EnhancedSwarmElection(n_voters=10, n_candidates=3)
    results, stability = election.run_full_election()
    
    print("\n" + "=" * 70)
    print("E8 Integration Demo Complete")
    print("=" * 70)
    print("\nNext Steps:")
    print("1. Integrate with your A11 master coordinator")
    print("2. Add E8 features to your 92% accurate voter ML")
    print("3. Render E8 roots in Babylon.js VR")
    print("4. Use p-adic heights for partition detection (β₀ > 1)")
    print("=" * 70)
