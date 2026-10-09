# Marvel Champions Deck Clustering Analysis

This Python script performs cluster analysis on MarvelCDB decklists to identify archetypal deck patterns and generate representative decklists for each cluster.

## Features

- **K-means Clustering**: Groups decks into 8 archetypal clusters
- **Feature Engineering**: Combines card usage patterns with metadata (aspect, hero, deck size)
- **Archetypal Deck Generation**: Creates representative decklists for each cluster
- **Comprehensive Analysis**: Provides detailed statistics and insights
- **Multiple Output Formats**: JSON data, text reports, and visualizations

## Installation

1. Install Python dependencies:

```bash
pip install -r requirements.txt
```

## Usage

### Basic Usage

```bash
python deck_clustering.py shadowcat_decklists_2024-01-01.json
```

### Advanced Usage

```bash
# Specify number of clusters
python deck_clustering.py shadowcat_decklists_2024-01-01.json --clusters 10

# Specify output directory
python deck_clustering.py shadowcat_decklists_2024-01-01.json --output my_results
```

### Programmatic Usage

```python
from deck_clustering import DeckClusterAnalyzer

# Create analyzer
analyzer = DeckClusterAnalyzer(n_clusters=8)

# Run analysis
cluster_analysis, archetypal_decks = analyzer.run_analysis(
    json_file="shadowcat_decklists_2024-01-01.json",
    output_dir="clustering_results"
)
```

## Input Format

The script expects a JSON file with the following structure:

```json
{
  "scrapedAt": "2024-01-01T12:00:00.000Z",
  "count": 100,
  "decklists": [
    {
      "id": "54439",
      "name": "Phase, Daze, Repeat!",
      "description": "...",
      "hero": "32030a",
      "aspect": "Justice",
      "cards": [
        {
          "code": "01001",
          "quantity": 1,
          "cost": 2
        }
      ]
    }
  ]
}
```

## Output

The script generates several output files:

### 1. `cluster_analysis.json`

Detailed analysis of each cluster:

```json
{
  "0": {
    "size": 15,
    "aspects": { "Justice": 12, "Leadership": 3 },
    "heroes": { "32030a": 15 },
    "common_cards": { "01001": 15, "01002": 12 },
    "avg_deck_size": 45.2,
    "avg_unique_cards": 28.5
  }
}
```

### 2. `archetypal_decks.json`

Representative decklists for each cluster:

```json
[
  {
    "cluster_id": 0,
    "cluster_size": 15,
    "name": "Archetype 1: Justice Shadowcat",
    "aspect": "Justice",
    "hero": "32030a",
    "description": "Generated from 15 decks in cluster 1",
    "cards": [
      {
        "code": "01001",
        "quantity": 1,
        "frequency": 15,
        "percentage": 100.0
      }
    ]
  }
]
```

### 3. `clustering_summary.txt`

Human-readable summary of the analysis.

## Algorithm Details

### Feature Engineering

The script creates feature vectors from:

1. **Card Usage Patterns**: Binary indicators for each card (present/absent)
2. **Metadata Features**:
   - Aspect (one-hot encoded)
   - Hero (one-hot encoded)
   - Deck size (total cards)
   - Unique card count
   - Average card cost

### Clustering Process

1. **Data Preprocessing**: Standardize features and apply PCA for dimensionality reduction
2. **K-means Clustering**: Group decks into k clusters using scikit-learn
3. **Cluster Analysis**: Analyze characteristics of each cluster
4. **Archetypal Generation**: Create representative decks based on most common cards

### Quality Metrics

- **Silhouette Score**: Measures cluster quality (-1 to 1, higher is better)
- **Cluster Size Distribution**: Ensures balanced clusters
- **Card Frequency Analysis**: Identifies core cards for each archetype

## Customization

### Adjusting Cluster Count

```python
analyzer = DeckClusterAnalyzer(n_clusters=10)  # 10 clusters instead of 8
```

### Modifying Feature Extraction

Edit the `extract_card_features()` and `extract_metadata_features()` methods to:

- Include/exclude specific card types
- Add new metadata features
- Change feature weighting

### Custom Archetypal Deck Generation

Modify `generate_archetypal_deck()` to:

- Change the number of top cards included
- Adjust quantity calculation logic
- Add custom naming conventions

## Example Results

After running the analysis, you might see output like:

```
Cluster 1: 15 decks
  Aspect: Justice
  Hero: 32030a
  Top cards: 01001, 01002, 01003

Cluster 2: 12 decks
  Aspect: Protection
  Hero: 32030a
  Top cards: 01004, 01005, 01006
```

## Troubleshooting

### Common Issues

1. **Empty decklists**: Ensure your JSON file contains valid deck data
2. **Memory issues**: Reduce the number of clusters or use fewer features
3. **Poor clustering**: Try different numbers of clusters or adjust feature weights

### Performance Tips

- Use PCA for dimensionality reduction with large card sets
- Consider sampling decks if you have thousands of decklists
- Adjust the `top_n_cards` parameter in archetypal deck generation

## Dependencies

- `numpy`: Numerical computations
- `pandas`: Data manipulation
- `scikit-learn`: Machine learning algorithms
- `matplotlib`: Plotting and visualization
- `seaborn`: Statistical data visualization

## License

This script is provided as-is for educational and research purposes.
