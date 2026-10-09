#!/usr/bin/env python3
"""
Marvel Champions Deck Clustering Analysis

This script performs cluster analysis on MarvelCDB decklists to identify
archetypal deck patterns and generate representative decklists for each cluster.
"""

import json
import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.metrics import silhouette_score
import matplotlib.pyplot as plt
import seaborn as sns
from collections import Counter, defaultdict
import argparse
import os
from typing import Dict, List, Tuple, Any

class DeckClusterAnalyzer:
    def __init__(self, n_clusters: int = 8, min_card_frequency: float = 0.0, max_card_frequency: float = 0.95):
        self.n_clusters = n_clusters
        self.min_card_frequency = min_card_frequency  # Minimum % of decks a card must appear in
        self.max_card_frequency = max_card_frequency  # Maximum % of decks a card can appear in
        self.scaler = StandardScaler()
        self.pca = PCA(n_components=0.95)  # Keep 95% of variance
        self.kmeans = KMeans(n_clusters=n_clusters, random_state=42, n_init=10)
        self.deck_data = None
        self.feature_matrix = None
        self.cluster_labels = None
        self.card_features = None
        
    def load_decklists(self, json_file: str) -> List[Dict]:
        """Load decklists from JSON file"""
        print(f"Loading decklists from {json_file}...")
        
        with open(json_file, 'r') as f:
            data = json.load(f)
        
        decklists = data.get('decklists', [])
        print(f"Loaded {len(decklists)} decklists")
        
        # Filter out decks without cards (check both 'cards' and 'slots' formats)
        valid_decks = []
        for deck in decklists:
            has_cards = False
            if deck.get('cards') and len(deck['cards']) > 0:
                has_cards = True
            elif deck.get('slots') and len(deck['slots']) > 0:
                has_cards = True
            
            if has_cards:
                valid_decks.append(deck)
        
        print(f"Found {len(valid_decks)} decks with cards")
        
        return valid_decks
    
    def extract_card_features(self, decklists: List[Dict]) -> Tuple[np.ndarray, List[str]]:
        """Extract card-based features from decklists"""
        print("Extracting card features...")
        
        # Collect all unique cards and their frequencies
        card_frequencies = Counter()
        for deck in decklists:
            deck_cards = set()
            
            # Handle 'cards' format
            for card in deck.get('cards', []):
                if isinstance(card, dict) and 'code' in card:
                    deck_cards.add(card['code'])
            
            # Handle 'slots' format
            slots = deck.get('slots', {})
            if isinstance(slots, dict):
                deck_cards.update(slots.keys())
            
            # Count how many decks each card appears in
            for card_code in deck_cards:
                card_frequencies[card_code] += 1
        
        # Filter out cards that appear in all decks (or nearly all)
        total_decks = len(decklists)
        min_decks_for_inclusion = max(1, int(total_decks * self.min_card_frequency))
        max_decks_for_inclusion = int(total_decks * self.max_card_frequency)
        
        # Detect hero-specific cards by finding cards that appear in most decks
        # Hero-specific cards typically appear in 80%+ of decks for a single hero
        hero_specific_threshold = total_decks * 0.8
        hero_specific_cards = [card for card, freq in card_frequencies.items() if freq >= hero_specific_threshold]
        
        print(f"Debug: Total decks = {total_decks}")
        print(f"Debug: Min frequency = {min_decks_for_inclusion}, Max frequency = {max_decks_for_inclusion}")
        print(f"Debug: 90% threshold = {total_decks * 0.9}")
        print(f"Debug: Hero-specific threshold (80%) = {hero_specific_threshold}")
        
        filtered_cards = []
        hero_specific_filtered = []
        universal_filtered = []
        
        for card_code, frequency in card_frequencies.items():
            # Skip hero-specific cards (appear in 80%+ of decks)
            if frequency >= hero_specific_threshold:
                hero_specific_filtered.append(card_code)
                continue
                
            # Skip cards that appear in all decks (90%+ threshold)
            if frequency >= total_decks * 0.9:
                universal_filtered.append(card_code)
                continue
                
            if min_decks_for_inclusion <= frequency <= max_decks_for_inclusion:
                filtered_cards.append(card_code)
        
        print(f"Debug: Hero-specific cards filtered: {hero_specific_filtered[:10]}{'...' if len(hero_specific_filtered) > 10 else ''}")
        print(f"Debug: Universal cards filtered: {universal_filtered[:10]}{'...' if len(universal_filtered) > 10 else ''}")
        print(f"Debug: Cards kept for clustering: {len(filtered_cards)}")
        
        card_list = sorted(filtered_cards)
        print(f"Found {len(card_frequencies)} total unique cards")
        print(f"Filtered to {len(card_list)} cards (removed {len(card_frequencies) - len(card_list)} universal/rare cards)")
        print(f"Card frequency filter: {self.min_card_frequency*100:.0f}% - {self.max_card_frequency*100:.0f}% of decks")
        
        # Print some examples of filtered cards
        universal_cards = [card for card, freq in card_frequencies.items() if freq > max_decks_for_inclusion]
        rare_cards = [card for card, freq in card_frequencies.items() if freq < min_decks_for_inclusion]
        very_universal_cards = [card for card, freq in card_frequencies.items() if freq >= total_decks * 0.9]
        
        if universal_cards:
            print(f"Universal cards (in >{self.max_card_frequency*100:.0f}% of decks): {universal_cards[:5]}{'...' if len(universal_cards) > 5 else ''}")
        if rare_cards:
            print(f"Rare cards (in <{self.min_card_frequency*100:.0f}% of decks): {rare_cards[:5]}{'...' if len(rare_cards) > 5 else ''}")
        if very_universal_cards:
            print(f"Very universal cards (in ≥90% of decks): {very_universal_cards[:10]}{'...' if len(very_universal_cards) > 10 else ''}")
            print(f"  Total very universal cards: {len(very_universal_cards)}")
        
        # Show which hero-specific cards were filtered
        if hero_specific_filtered:
            print(f"Filtered hero-specific cards (≥80% frequency): {hero_specific_filtered[:10]}{'...' if len(hero_specific_filtered) > 10 else ''}")
            print(f"  Total hero-specific cards filtered: {len(hero_specific_filtered)}")
        
        # Create feature matrix
        n_decks = len(decklists)
        n_cards = len(card_list)
        feature_matrix = np.zeros((n_decks, n_cards))
        
        # Fill feature matrix
        for i, deck in enumerate(decklists):
            card_counts = Counter()
            
            # Handle 'cards' format
            for card in deck.get('cards', []):
                if isinstance(card, dict) and 'code' in card:
                    card_counts[card['code']] += card.get('quantity', 1)
            
            # Handle 'slots' format
            slots = deck.get('slots', {})
            if isinstance(slots, dict):
                for card_code, quantity in slots.items():
                    card_counts[card_code] += quantity
            
            for j, card_code in enumerate(card_list):
                feature_matrix[i, j] = card_counts.get(card_code, 0)
        
        return feature_matrix, card_list
    
    def detect_aspect_from_meta(self, deck: Dict) -> str:
        """Detect aspect from meta field in the deck"""
        meta_str = deck.get('meta', '{}')
        try:
            meta = json.loads(meta_str)
            aspect = meta.get('aspect', '').title()  # Convert to title case
            if aspect in ['Aggression', 'Justice', 'Leadership', 'Protection']:
                return aspect
        except (json.JSONDecodeError, AttributeError):
            pass
        
        return 'Unknown'

    def extract_metadata_features(self, decklists: List[Dict]) -> np.ndarray:
        """Extect metadata features (aspect, hero, etc.)"""
        print("Extracting metadata features...")
        
        # Define feature categories
        aspects = ['Aggression', 'Justice', 'Leadership', 'Protection']
        heroes = set()
        
        # Collect all heroes
        for deck in decklists:
            hero = deck.get('hero') or deck.get('hero_code') or deck.get('hero_name')
            if hero:
                heroes.add(hero)
        
        hero_list = sorted(list(heroes))
        
        # Create feature matrix
        n_decks = len(decklists)
        n_features = len(aspects) + len(hero_list) + 3  # +3 for deck size, unique cards, avg cost
        
        feature_matrix = np.zeros((n_decks, n_features))
        
        for i, deck in enumerate(decklists):
            feature_idx = 0
            
            # Aspect features (one-hot encoding) - detect from meta field
            aspect = self.detect_aspect_from_meta(deck)
            for aspect_name in aspects:
                feature_matrix[i, feature_idx] = 1 if aspect_name == aspect else 0
                feature_idx += 1
            
            # Hero features (one-hot encoding)
            hero = deck.get('hero') or deck.get('hero_code') or deck.get('hero_name') or ''
            for hero_name in hero_list:
                feature_matrix[i, feature_idx] = 1 if hero_name == hero else 0
                feature_idx += 1
            
            # Deck size
            total_cards = 0
            unique_cards = 0
            
            # Handle 'cards' format
            cards = deck.get('cards', [])
            if cards:
                total_cards = sum(card.get('quantity', 1) for card in cards if isinstance(card, dict))
                unique_cards = len(set(card.get('code', '') for card in cards if isinstance(card, dict) and card.get('code')))
            
            # Handle 'slots' format
            slots = deck.get('slots', {})
            if isinstance(slots, dict):
                total_cards = sum(slots.values())
                unique_cards = len(slots.keys())
            
            feature_matrix[i, feature_idx] = total_cards
            feature_idx += 1
            
            feature_matrix[i, feature_idx] = unique_cards
            feature_idx += 1
            
            # Average cost (not available in slots format, set to 0)
            feature_matrix[i, feature_idx] = 0
        
        return feature_matrix
    
    def prepare_features(self, decklists: List[Dict]) -> np.ndarray:
        """Prepare feature matrix for clustering"""
        print("Preparing features for clustering...")
        
        # Extract card features
        card_features, self.card_features = self.extract_card_features(decklists)
        
        # Extract metadata features
        metadata_features = self.extract_metadata_features(decklists)
        
        # Combine features
        combined_features = np.hstack([card_features, metadata_features])
        
        print(f"Feature matrix shape: {combined_features.shape}")
        print(f"Card features: {card_features.shape[1]}")
        print(f"Metadata features: {metadata_features.shape[1]}")
        
        return combined_features
    
    def perform_clustering(self, features: np.ndarray) -> np.ndarray:
        """Perform K-means clustering"""
        print(f"Performing K-means clustering with {self.n_clusters} clusters...")
        
        # Scale features
        features_scaled = self.scaler.fit_transform(features)
        
        # Apply PCA for dimensionality reduction
        features_pca = self.pca.fit_transform(features_scaled)
        print(f"PCA reduced dimensions to: {features_pca.shape[1]}")
        
        # Perform clustering
        cluster_labels = self.kmeans.fit_predict(features_pca)
        
        # Calculate silhouette score
        silhouette_avg = silhouette_score(features_pca, cluster_labels)
        print(f"Silhouette Score: {silhouette_avg:.3f}")
        
        return cluster_labels
    
    def analyze_clusters(self, decklists: List[Dict], cluster_labels: np.ndarray, filtered_cards: List[str] = None) -> Dict:
        """Analyze cluster characteristics"""
        print("Analyzing cluster characteristics...")
        
        cluster_analysis = {}
        
        for cluster_id in range(self.n_clusters):
            cluster_decks = [deck for i, deck in enumerate(decklists) if cluster_labels[i] == cluster_id]
            
            if not cluster_decks:
                continue
            
            # Basic stats
            cluster_analysis[cluster_id] = {
                'size': len(cluster_decks),
                'decks': cluster_decks,
                'deck_ids': [deck.get('id') for deck in cluster_decks if deck.get('id')],
                'aspects': Counter(),
                'heroes': Counter(),
                'common_cards': Counter(),
                'avg_deck_size': 0,
                'avg_unique_cards': 0
            }
            
            # Analyze aspects and heroes
            for deck in cluster_decks:
                aspect = self.detect_aspect_from_meta(deck)
                hero = deck.get('hero') or deck.get('hero_code') or deck.get('hero_name') or 'Unknown'
                cluster_analysis[cluster_id]['aspects'][aspect] += 1
                cluster_analysis[cluster_id]['heroes'][hero] += 1
            
            # Analyze cards
            all_cards = []
            deck_sizes = []
            unique_card_counts = []
            
            for deck in cluster_decks:
                deck_size = 0
                unique_cards = 0
                
                # Handle 'cards' format
                cards = deck.get('cards', [])
                if cards:
                    deck_size = sum(card.get('quantity', 1) for card in cards if isinstance(card, dict))
                    unique_cards = len(set(card.get('code', '') for card in cards if isinstance(card, dict) and card.get('code')))
                    
                    for card in cards:
                        if isinstance(card, dict) and 'code' in card:
                            # Only include cards that are in the filtered list (if provided)
                            if filtered_cards is None or card['code'] in filtered_cards:
                                all_cards.append(card['code'])
                
                # Handle 'slots' format
                slots = deck.get('slots', {})
                if isinstance(slots, dict):
                    deck_size = sum(slots.values())
                    unique_cards = len(slots.keys())
                    
                    for card_code, quantity in slots.items():
                        # Only include cards that are in the filtered list (if provided)
                        if filtered_cards is None or card_code in filtered_cards:
                            for _ in range(quantity):
                                all_cards.append(card_code)
                
                deck_sizes.append(deck_size)
                unique_card_counts.append(unique_cards)
            
            cluster_analysis[cluster_id]['common_cards'] = Counter(all_cards)
            cluster_analysis[cluster_id]['avg_deck_size'] = np.mean(deck_sizes)
            cluster_analysis[cluster_id]['avg_unique_cards'] = np.mean(unique_card_counts)
        
        return cluster_analysis
    
    def generate_archetypal_deck(self, cluster_id: int, cluster_analysis: Dict, decklists: List[Dict], top_n_cards: int = 30) -> Dict:
        """Generate an archetypal deck for a cluster"""
        if cluster_id not in cluster_analysis:
            return {}
        
        cluster_data = cluster_analysis[cluster_id]
        
        # Get most common aspect and hero
        most_common_aspect = cluster_data['aspects'].most_common(1)[0][0] if cluster_data['aspects'] else 'Unknown'
        most_common_hero = cluster_data['heroes'].most_common(1)[0][0] if cluster_data['heroes'] else 'Unknown'
        
        # Get all cards from decks in this cluster (including filtered cards)
        all_cluster_cards = Counter()
        for deck in cluster_data['decks']:
            # Handle 'slots' format
            slots = deck.get('slots', {})
            if isinstance(slots, dict):
                for card_code, quantity in slots.items():
                    all_cluster_cards[card_code] += quantity
            
            # Handle 'cards' format
            cards = deck.get('cards', [])
            if cards:
                for card in cards:
                    if isinstance(card, dict) and 'code' in card:
                        all_cluster_cards[card['code']] += card.get('quantity', 1)
        
        # Get top cards from all cards (not just filtered ones)
        top_cards = all_cluster_cards.most_common(top_n_cards)
        
        # Create archetypal deck
        archetypal_deck = {
            'cluster_id': cluster_id,
            'cluster_size': cluster_data['size'],
            'name': f"Archetype {cluster_id + 1}: {most_common_aspect} {most_common_hero}",
            'aspect': most_common_aspect,
            'hero': most_common_hero,
            'description': f"Generated from {cluster_data['size']} decks in cluster {cluster_id + 1}",
            'avg_deck_size': cluster_data['avg_deck_size'],
            'avg_unique_cards': cluster_data['avg_unique_cards'],
            'cards': []
        }
        
        # Add top cards
        for card_code, frequency in top_cards:
            # Calculate average quantity (frequency / number of decks in cluster)
            avg_quantity = max(1, round(frequency / cluster_data['size']))
            archetypal_deck['cards'].append({
                'code': card_code,
                'quantity': avg_quantity,
                'frequency': frequency,
                'percentage': (frequency / cluster_data['size']) * 100
            })
        
        return archetypal_deck
    
    def save_results(self, cluster_analysis: Dict, archetypal_decks: List[Dict], output_dir: str):
        """Save clustering results"""
        os.makedirs(output_dir, exist_ok=True)
        
        # Save cluster analysis
        with open(f"{output_dir}/cluster_analysis.json", 'w') as f:
            # Convert to JSON-serializable format
            json_data = {}
            for cluster_id, data in cluster_analysis.items():
                json_data[str(cluster_id)] = {
                    'size': data['size'],
                    'deck_ids': data['deck_ids'],
                    'aspects': dict(data['aspects']),
                    'heroes': dict(data['heroes']),
                    'common_cards': dict(data['common_cards'].most_common(50)),
                    'avg_deck_size': data['avg_deck_size'],
                    'avg_unique_cards': data['avg_unique_cards']
                }
            json.dump(json_data, f, indent=2)
        
        # Save archetypal decks
        with open(f"{output_dir}/archetypal_decks.json", 'w') as f:
            json.dump(archetypal_decks, f, indent=2)
        
        # Save summary report
        with open(f"{output_dir}/clustering_summary.txt", 'w') as f:
            f.write("Marvel Champions Deck Clustering Analysis\n")
            f.write("=" * 50 + "\n\n")
            
            f.write(f"Total clusters: {self.n_clusters}\n")
            f.write(f"Total decks analyzed: {sum(data['size'] for data in cluster_analysis.values())}\n\n")
            
            for cluster_id in range(self.n_clusters):
                if cluster_id in cluster_analysis:
                    data = cluster_analysis[cluster_id]
                    f.write(f"Cluster {cluster_id + 1}:\n")
                    f.write(f"  Size: {data['size']} decks\n")
                    f.write(f"  Most common aspect: {data['aspects'].most_common(1)[0][0] if data['aspects'] else 'Unknown'}\n")
                    f.write(f"  Most common hero: {data['heroes'].most_common(1)[0][0] if data['heroes'] else 'Unknown'}\n")
                    f.write(f"  Avg deck size: {data['avg_deck_size']:.1f} cards\n")
                    f.write(f"  Avg unique cards: {data['avg_unique_cards']:.1f}\n")
                    f.write(f"  Top 5 cards: {', '.join([card for card, _ in data['common_cards'].most_common(5)])}\n\n")
        
        print(f"Results saved to {output_dir}/")
    
    def run_analysis(self, json_file: str, output_dir: str = "clustering_results"):
        """Run complete clustering analysis"""
        print("Starting Marvel Champions Deck Clustering Analysis")
        print("=" * 50)
        
        # Load data
        decklists = self.load_decklists(json_file)
        if not decklists:
            print("No valid decklists found!")
            return
        
        # Prepare features
        features = self.prepare_features(decklists)
        self.feature_matrix = features
        
        # Perform clustering
        cluster_labels = self.perform_clustering(features)
        self.cluster_labels = cluster_labels
        
        # Analyze clusters
        cluster_analysis = self.analyze_clusters(decklists, cluster_labels, self.card_features)
        
        # Generate archetypal decks
        archetypal_decks = []
        for cluster_id in range(self.n_clusters):
            archetypal_deck = self.generate_archetypal_deck(cluster_id, cluster_analysis, decklists)
            if archetypal_deck:
                archetypal_decks.append(archetypal_deck)
        
        # Save results
        self.save_results(cluster_analysis, archetypal_decks, output_dir)
        
        # Print summary
        print("\n" + "=" * 50)
        print("CLUSTERING SUMMARY")
        print("=" * 50)
        
        for cluster_id in range(self.n_clusters):
            if cluster_id in cluster_analysis:
                data = cluster_analysis[cluster_id]
                print(f"\nCluster {cluster_id + 1}: {data['size']} decks")
                print(f"  Aspect: {data['aspects'].most_common(1)[0][0] if data['aspects'] else 'Unknown'}")
                print(f"  Hero: {data['heroes'].most_common(1)[0][0] if data['heroes'] else 'Unknown'}")
                print(f"  Top cards: {', '.join([card for card, _ in data['common_cards'].most_common(3)])}")
        
        print(f"\nResults saved to {output_dir}/")
        return cluster_analysis, archetypal_decks

def main():
    parser = argparse.ArgumentParser(description='Cluster Marvel Champions decklists')
    parser.add_argument('json_file', help='JSON file containing decklists')
    parser.add_argument('--clusters', type=int, default=8, help='Number of clusters (default: 8)')
    parser.add_argument('--output', default='clustering_results', help='Output directory (default: clustering_results)')
    parser.add_argument('--min-card-freq', type=float, default=0.1, help='Minimum card frequency (0.0-1.0, default: 0.1)')
    parser.add_argument('--max-card-freq', type=float, default=0.95, help='Maximum card frequency (0.0-1.0, default: 0.95)')
    
    args = parser.parse_args()
    
    if not os.path.exists(args.json_file):
        print(f"Error: File {args.json_file} not found!")
        return
    
    analyzer = DeckClusterAnalyzer(
        n_clusters=args.clusters,
        min_card_frequency=args.min_card_freq,
        max_card_frequency=args.max_card_freq
    )
    analyzer.run_analysis(args.json_file, args.output)

if __name__ == "__main__":
    main()
