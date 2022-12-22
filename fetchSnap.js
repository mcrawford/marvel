import fetch from 'node-fetch'
import {format} from '@fast-csv/format'
import fs from 'fs'

// Electro: 75
// Sandman: 215
// Ultron: 281
// Kingpin: 119
// Crossbones: 46
// Red Skull: 203
// Deadpool: 56
// Hellcow: 97
// Quake: 197
// Crystal: 47
// Rescue: 204
// Omega Red: 187
// Debrii: 59
// Adam Warlock: 3
// Leech: 131
// Namor: 175
// Baron Mordo: 18
// Cerebro: 39
// Zero: 311
// Abomination: 1

const decks = await loadDecks(1)
// decks.push(...await loadDecks())

const deckLists = []
const cardNames = []
decks.forEach(d => {
    const cards = d.deck.decklist.cards
    const cardIds = []
    cards.forEach(card => {
        cardNames[card.cid] = card.cname
        cardIds.push(String(card.cid))
    })

    deckLists.push({
        id: d.deck.info.did,
        name: d.deck.info.name,
        cardIds: cardIds
    })
})
// console.log(cardNames)

const matrix = deckLists.map(dl => {
    const flags = Object.entries(cardNames).reduce((map, [id, name]) => {
        map[name] = dl.cardIds.includes(id) ? 1 : 0
        return map
    }, {})
    return {
        id: dl.id,
        name: dl.name,
        ...flags
    }
})
console.log(`Got ${matrix.length} decks`)
// console.log(cardNames)

const ws = fs.createWriteStream('snapdecks.csv')
const csvStream = format({headers: ['id', 'name', ...cardNames]})
csvStream.pipe(ws).on('end', () => process.exit())
matrix.forEach(row => {
    csvStream.write(row)
})
csvStream.end()


async function loadDecks(cardId) {
    const decks = []
    for (let i = 0; i < 10; i++) {
        let body = `------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="action"

api_actions
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="searchdecks"

true
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="tags"

[]
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="cardtags"

[]
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="deckname"


------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="abilities"

[]
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="sources"

[]
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="cards"

[${cardId}]
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="collection"

[]
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="onlywithlikes"

0
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="onlywithvideo"

0
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="onlycontentcreators"

0
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="onlynonanonymous"

0
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="sorttype"

updated
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="sortorder"

asc
------WebKitFormBoundarynuz5JQGPBGiFQ1ec
Content-Disposition: form-data; name="nextpage"

${i}
------WebKitFormBoundarynuz5JQGPBGiFQ1ec--`
        const resp = await fetch('https://marvelsnapzone.com/wp-admin/admin-ajax.php', {
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
            'body': body,
            'method': 'POST'
        })
        const data = await resp.json()
        console.log(`...got ${data?.success?.decks?.length} decks for ${cardId}...`)
        if (data?.success?.decks?.length) {
            decks.push(...data?.success?.decks)
        } else {
            break
        }
    }

    return decks
}