# MarvelCDB Scraper

A TypeScript script to scrape decklists from MarvelCDB.com for Marvel Champions: The Card Game.

## Features

- Scrapes decklist links from hero pages
- Fetches full decklist data from the MarvelCDB API
- Includes rate limiting to be respectful to the server
- Saves results to JSON files
- Error handling for failed requests

## Installation

1. Install dependencies:
```bash
npm install
```

2. Compile TypeScript (optional, can run directly with ts-node):
```bash
npx tsc
```

## Usage

### Basic Usage

Run the main scraper script:
```bash
npx tsx scrapeMarvelCDB.ts
```

This will scrape all Shadowcat decklists (hero ID: 32030a) and save them to a JSON file.

### Custom Hero

To scrape a different hero, modify the `heroId` variable in `scrapeMarvelCDB.ts`:

```typescript
const heroId = 'your-hero-id-here';
```

### Programmatic Usage

```typescript
import { MarvelCDBScraper } from './scrapeMarvelCDB.js';

const scraper = new MarvelCDBScraper();

// Scrape all decklists for a hero
const decklists = await scraper.scrapeHeroDecklists('32030a');

// Get just the decklist links
const links = await scraper.scrapeDecklistLinks('32030a');

// Fetch a specific decklist
const decklist = await scraper.fetchDecklistData('54439');

// Save to file
scraper.saveDecklistsToFile(decklists, 'output.json');
```

### Example Usage

Run the example script to see different usage patterns:
```bash
npx tsx example-usage.ts
```

## API Reference

### MarvelCDBScraper Class

#### Methods

- `scrapeDecklistLinks(heroId: string): Promise<DecklistLink[]>`
  - Scrapes all decklist links from a hero's decklist page
  - Returns array of decklist links with ID, URL, and title

- `fetchDecklistData(decklistId: string): Promise<DecklistData | null>`
  - Fetches full decklist data from the MarvelCDB API
  - Returns null if decklist not found (404)

- `scrapeHeroDecklists(heroId: string): Promise<DecklistData[]>`
  - Complete workflow: scrapes links then fetches all decklist data
  - Includes rate limiting and error handling

- `saveDecklistsToFile(decklists: DecklistData[], filename: string): void`
  - Saves decklists to a JSON file with metadata

#### Properties

- `delay: number` - Delay between requests in milliseconds (default: 1000)

### Data Types

#### DecklistLink
```typescript
interface DecklistLink {
  id: string;
  url: string;
  title: string;
}
```

#### DecklistData
```typescript
interface DecklistData {
  id: string;
  name: string;
  description: string;
  hero: string;
  aspect: string;
  cards: any[];
  [key: string]: any;
}
```

## Rate Limiting

The scraper includes a 1-second delay between requests to be respectful to the MarvelCDB server. You can adjust this by modifying the `delay` property:

```typescript
const scraper = new MarvelCDBScraper();
scraper.delay = 2000; // 2 seconds between requests
```

## Error Handling

The scraper includes comprehensive error handling:

- Network errors are caught and logged
- 404 errors for missing decklists are handled gracefully
- Individual decklist failures don't stop the entire scraping process
- All errors are logged with context

## Output Format

The JSON output includes:

```json
{
  "scrapedAt": "2024-01-01T12:00:00.000Z",
  "count": 10,
  "decklists": [
    {
      "id": "54439",
      "name": "Phase, Daze, Repeat!",
      "description": "...",
      "hero": "32030a",
      "aspect": "Justice",
      "cards": [...],
      ...
    }
  ]
}
```

## Notes

- The script respects robots.txt and includes appropriate delays
- User-Agent header is set to mimic a browser
- All requests include proper error handling
- The MarvelCDB API is public and doesn't require authentication

## Troubleshooting

### Common Issues

1. **Network errors**: Check your internet connection and try again
2. **Rate limiting**: Increase the delay between requests if you get 429 errors
3. **Missing decklists**: Some decklists may be private or deleted, these are skipped
4. **TypeScript errors**: Make sure all dependencies are installed with `npm install`

### Debug Mode

To see more detailed logging, you can modify the console.log statements or add additional logging in the script.
