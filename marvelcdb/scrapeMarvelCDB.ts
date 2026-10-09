import axios from "axios";
import * as cheerio from "cheerio";
import { writeFileSync } from "fs";

interface DecklistLink {
  id: string;
  url: string;
  title: string;
}

interface DecklistData {
  id: string;
  name: string;
  description: string;
  hero: string;
  aspect: string;
  cards: any[];
  [key: string]: any;
}

class MarvelCDBScraper {
  private baseUrl = "https://marvelcdb.com";
  private apiBaseUrl = "https://marvelcdb.com/api/public";
  private delay = 1000; // 1 second delay between requests

  constructor() {
    // Set up axios with proper headers to mimic a browser
    axios.defaults.headers.common["User-Agent"] =
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36";
  }

  /**
   * Scrape decklist links from a specific page
   */
  async scrapeDecklistLinksFromPage(
    heroId: string,
    page: number = 1
  ): Promise<DecklistLink[]> {
    const url = `${this.baseUrl}/decklists/all/${page}?hero=${heroId}`;
    console.log(`Scraping decklist links from page ${page}: ${url}`);

    try {
      const response = await axios.get(url);
      const $ = cheerio.load(response.data);
      const decklistLinks: DecklistLink[] = [];

      // Find all decklist links - they follow the pattern /decklist/view/{id}/{slug}
      $('a[href*="/decklist/view/"]').each((index: number, element: any) => {
        const href = $(element).attr("href");
        if (href) {
          // Extract decklist ID from URL like /decklist/view/54439/phase-daze-repeat-1.0
          const match = href.match(/\/decklist\/view\/(\d+)\//);
          if (match) {
            const id = match[1];
            const title = $(element).text().trim();
            decklistLinks.push({
              id,
              url: `${this.baseUrl}${href}`,
              title,
            });
          }
        }
      });

      console.log(
        `Found ${decklistLinks.length} decklist links on page ${page}`
      );
      return decklistLinks;
    } catch (error) {
      console.error(`Error scraping decklist links from page ${page}:`, error);
      throw error;
    }
  }

  /**
   * Get the total number of pages for a hero's decklists
   */
  async getTotalPages(heroId: string): Promise<number> {
    const url = `${this.baseUrl}/decklists/all/1?hero=${heroId}`;
    console.log(`Getting total pages from: ${url}`);

    try {
      const response = await axios.get(url);
      const $ = cheerio.load(response.data);

      // Look for pagination links to find the highest page number
      let maxPage = 1;
      $('a[href*="/decklists/all/"]').each((index: number, element: any) => {
        const href = $(element).attr("href");
        if (href) {
          const match = href.match(/\/decklists\/all\/(\d+)/);
          if (match) {
            const pageNum = parseInt(match[1]);
            if (pageNum > maxPage) {
              maxPage = pageNum;
            }
          }
        }
      });

      console.log(`Found ${maxPage} total pages for hero ${heroId}`);
      return maxPage;
    } catch (error) {
      console.error("Error getting total pages:", error);
      return 1; // Default to 1 page if we can't determine
    }
  }

  /**
   * Scrape decklist links from multiple pages for a hero
   */
  async scrapeDecklistLinks(
    heroId: string,
    maxPages: number = 10
  ): Promise<DecklistLink[]> {
    console.log(
      `Starting to scrape decklist links for hero: ${heroId} (max ${maxPages} pages)`
    );

    // Get total pages available, but limit to maxPages
    const availablePages = await this.getTotalPages(heroId);
    const totalPages = Math.min(maxPages, availablePages);

    console.log(`Scraping ${totalPages} pages (${availablePages} available)`);

    const allDecklistLinks: DecklistLink[] = [];
    const seenIds = new Set<string>(); // To avoid duplicates

    for (let page = 1; page <= totalPages; page++) {
      console.log(`\n--- Processing page ${page}/${totalPages} ---`);

      try {
        const pageLinks = await this.scrapeDecklistLinksFromPage(heroId, page);

        // Add unique links (avoid duplicates across pages)
        for (const link of pageLinks) {
          if (!seenIds.has(link.id)) {
            seenIds.add(link.id);
            allDecklistLinks.push(link);
          }
        }

        console.log(
          `Page ${page}: Found ${pageLinks.length} links, ${allDecklistLinks.length} total unique links`
        );

        // Add delay between page requests
        if (page < totalPages) {
          await this.delayRequest();
        }
      } catch (error) {
        console.error(`Failed to scrape page ${page}:`, error);
        // Continue with other pages even if one fails
      }
    }

    console.log(
      `\nTotal unique decklist links found: ${allDecklistLinks.length}`
    );
    return allDecklistLinks;
  }

  /**
   * Fetch decklist data from the API
   */
  async fetchDecklistData(decklistId: string): Promise<DecklistData | null> {
    const url = `${this.apiBaseUrl}/decklist/${decklistId}`;
    console.log(`Fetching decklist data from: ${url}`);

    try {
      const response = await axios.get(url);
      return response.data;
    } catch (error: unknown) {
      if (axios.isAxiosError(error) && error.response?.status === 404) {
        console.warn(`Decklist ${decklistId} not found (404)`);
        return null;
      }
      console.error(`Error fetching decklist ${decklistId}:`, error);
      throw error;
    }
  }

  /**
   * Add delay between requests to be respectful to the server
   */
  private async delayRequest(): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, this.delay));
  }

  /**
   * Scrape all decklists for a hero
   */
  async scrapeHeroDecklists(heroId: string): Promise<DecklistData[]> {
    console.log(`Starting to scrape decklists for hero: ${heroId}`);

    // Step 1: Get all decklist links
    const decklistLinks = await this.scrapeDecklistLinks(heroId);

    if (decklistLinks.length === 0) {
      console.log("No decklists found for this hero");
      return [];
    }

    // Step 2: Fetch data for each decklist
    const decklists: DecklistData[] = [];

    for (let i = 0; i < decklistLinks.length; i++) {
      const link = decklistLinks[i];
      console.log(
        `Processing decklist ${i + 1}/${decklistLinks.length}: ${
          link.title
        } (ID: ${link.id})`
      );

      try {
        const decklistData = await this.fetchDecklistData(link.id);
        if (decklistData) {
          decklists.push(decklistData);
        }
      } catch (error) {
        console.error(`Failed to fetch decklist ${link.id}:`, error);
        // Continue with other decklists even if one fails
      }

      // Add delay between requests (except for the last one)
      if (i < decklistLinks.length - 1) {
        await this.delayRequest();
      }
    }

    console.log(`Successfully scraped ${decklists.length} decklists`);
    return decklists;
  }

  /**
   * Save decklists to a JSON file
   */
  saveDecklistsToFile(decklists: DecklistData[], filename: string): void {
    const output = {
      scrapedAt: new Date().toISOString(),
      count: decklists.length,
      decklists,
    };

    writeFileSync(filename, JSON.stringify(output, null, 2));
    console.log(`Saved ${decklists.length} decklists to ${filename}`);
  }
}

// Main execution function
async function main() {
  const scraper = new MarvelCDBScraper();

  // Example: Scrape Shadowcat decklists (hero ID: 32030a)
  const heroId = "32030a";

  try {
    const decklists = await scraper.scrapeHeroDecklists(heroId);

    if (decklists.length > 0) {
      const filename = `shadowcat_decklists_${
        new Date().toISOString().split("T")[0]
      }.json`;
      scraper.saveDecklistsToFile(decklists, filename);

      // Print summary
      console.log("\n=== SCRAPING SUMMARY ===");
      console.log(`Hero ID: ${heroId}`);
      console.log(`Total decklists found: ${decklists.length}`);
      console.log(`Output file: ${filename}`);

      // Show first few decklist names as examples
      console.log("\nFirst few decklists:");
      decklists.slice(0, 5).forEach((decklist, index) => {
        console.log(
          `${index + 1}. ${decklist.name || "Untitled"} (ID: ${decklist.id})`
        );
      });
    } else {
      console.log("No decklists were successfully scraped");
    }
  } catch (error) {
    console.error("Scraping failed:", error);
    process.exit(1);
  }
}

// Run the script if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { MarvelCDBScraper, DecklistLink, DecklistData };
