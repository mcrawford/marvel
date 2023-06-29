import fetch from 'node-fetch'
import {format} from '@fast-csv/format'
import fs from 'fs'

const decks = await loadDecks('Okoye')

const deckLists = []
const cardNames = new Set()
decks.forEach(d => {
    d.deck = d.deck.map(n => n.replace(/_\d\d/, ''))
    d.deck.forEach(n => cardNames.add(n))
    deckLists.push({
        name: d.humanname,
        views: d.views,
        cards: d.deck
    })
})
// console.log(cardNames)
const matrix = deckLists.map(dl => {
    const flags = {}
    cardNames.forEach(name => {
        flags[name] = dl.cards.includes(name) ? 1 : 0
    })
    return {
        name: dl.name,
        views: dl.views,
        ...flags
    }
})
console.log(`Got ${matrix.length} decks`)
// console.log(matrix)

const ws = fs.createWriteStream('snap/snapdecks.csv')
const csvStream = format({headers: ['name', 'views', ...cardNames.values()]})
csvStream.pipe(ws).on('end', () => process.exit())
matrix.forEach(row => {
    csvStream.write(row)
})
csvStream.end()


async function loadDecks(cardName) {
    const decks = []
    for (let i = 1; i <= 10; i++) {
        const params = {
            cache: Date.now(),
            cmd: 'getdecks',
            rq: `{
                "page": ${i},
                "limit": 30,
                "srt": "date",
                "direct": "desc",
                "type": "",
                "my": 0,
                "myarchive": 0,
                "fav": 0,
                "getdecks": {
                    "hascrd": ["${cardName}"],
                    "nothascrd": [],
                    "youtube": 0,
                    "archetype": 0,
                    "supertype": "",
                    "smartsrch": "",
                    "pool": 0,
                    "date": "",
                    "collection": ""
                }
            }`
        }
        const paramStrings = []
        Object.entries(params).forEach(([k, v]) => {
            paramStrings.push(`${k}=${String(v).replace(/\s/g, '')}`)
        })
        // console.log(`https://marvelsnapzone.com/pro/do.php?${paramStrings.join('&')}`)
        const resp = await fetch(`https://marvelsnapzone.com/pro/do.php?${paramStrings.join('&')}`, {
            'headers': {
                'accept': 'application/json, text/plain, */*',
                'accept-language': 'en-US,en;q=0.8',
                'content-type': 'multipart/form-data; boundary=----WebKitFormBoundarynuz5JQGPBGiFQ1ec',
                'sec-fetch-dest': 'empty',
                'sec-fetch-mode': 'cors',
                'sec-fetch-site': 'same-origin',
                'sec-gpc': '1',
                'Referer': 'https://marvelsnapzone.com/decks/',
                'Referrer-Policy': 'strict-origin-when-cross-origin'
            },
        })
        const data = await resp.json()
        // console.log(data)
        console.log(`...got ${data?.length} decks for ${cardName}...`)
        if (data?.length) {
            decks.push(...data)
        } else {
            break
        }
    }

    return decks
}