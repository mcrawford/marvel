#!/usr/bin/env python3
"""
Example usage of the deck clustering analysis
"""

from deck_clustering import DeckClusterAnalyzer
import json
import os

def main():
    # Example: Analyze Shadowcat decklists
    json_file = "shadowcat_decklists_2024-01-01.json"  # Replace with your actual file
    
    if not os.path.exists(json_file):
        print(f"Error: {json_file} not found!")
        print("Please run the scraper first to generate decklist data.")
        return
    
    # Create analyzer with 8 clusters
    analyzer = DeckClusterAnalyzer(n_clusters=8)
    
    # Run analysis
    cluster_analysis, archetypal_decks = analyzer.run_analysis(
        json_file=json_file,
        output_dir="shadowcat_clustering_results"
    )
    
    # Print archetypal decks
    print("\n" + "=" * 60)
    print("ARCHETYPAL DECKS")
    print("=" * 60)
    
    for deck in archetypal_decks:
        print(f"\n{deck['name']}")
        print(f"Cluster size: {deck['cluster_size']} decks")
        print(f"Description: {deck['description']}")
        print(f"Average deck size: {deck['avg_deck_size']:.1f} cards")
        print(f"Average unique cards: {deck['avg_unique_cards']:.1f}")
        
        print("\nTop 10 cards:")
        for i, card in enumerate(deck['cards'][:10]):
            print(f"  {i+1:2d}. {card['code']} x{card['quantity']} ({card['percentage']:.1f}%)")
        
        print("-" * 40)

if __name__ == "__main__":
    main()
