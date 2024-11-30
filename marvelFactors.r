MarvelFactors <- function(extendedGlm) {
  coefs <- coef(extendedGlm)
  multiplayer <- c(
    0, # Rhino
    coefs["ScenarioKlaw"],
    coefs["ScenarioUltron"],
    coefs["ScenarioWrecking Crew"],
    coefs["ScenarioRisky Business"],
    coefs["ScenarioMutagen Formula"],
    coefs["ScenarioCrossbones"],
    coefs["ScenarioAbsorbing Man"],
    coefs["ScenarioTaskmaster"],
    coefs["ScenarioZola"],
    coefs["ScenarioRed Skull"],
    coefs["ScenarioKang"],
    coefs["ScenarioBrotherhood of Badoon"],
    coefs["ScenarioInfiltrate the Museum"],
    coefs["ScenarioEscape the Museum"],
    coefs["ScenarioNebula"],
    coefs["ScenarioRonan the Accuser"],
    coefs["ScenarioEbony Maw"],
    coefs["ScenarioTower Defense"],
    coefs["ScenarioThanos"],
    coefs["ScenarioHela"],
    coefs["ScenarioLoki"],
    coefs["ScenarioThe Hood"],
    coefs["ScenarioSandman"],
    coefs["ScenarioVenom"],
    coefs["ScenarioMysterio"],
    coefs["ScenarioThe Sinister Six"],
    coefs["ScenarioVenom Goblin"],
    coefs["ScenarioSabretooth"],
    coefs["ScenarioProject Wideawake"],
    coefs["ScenarioMaster Mold"],
    coefs["ScenarioMansion Attack"],
    coefs["ScenarioMagneto"],
    coefs["ScenarioMaGog"],
    coefs["ScenarioSpiral"],
    coefs["ScenarioMojo"],
    coefs["ScenarioMorlock Siege"],
    coefs["ScenarioOn the Run"],
    coefs["ScenarioJuggernaut"],
    coefs["ScenarioMister Sinister"],
    coefs["ScenarioStryfe"],
    coefs["ScenarioUnus"],
    coefs["ScenarioFour Horsemen"],
    coefs["ScenarioApocalypse"],
    coefs["ScenarioDark Beast"],
    coefs["ScenarioEn Sabah Nur"]
  )
  oneHero <- c(
    0 + coefs["OneHeroTRUE"], # Rhino
    coefs["ScenarioKlaw"] + coefs["OneHeroTRUE"] + coefs["ScenarioKlaw:OneHeroTRUE"],
    coefs["ScenarioUltron"] + coefs["OneHeroTRUE"] + coefs["ScenarioUltron:OneHeroTRUE"],
    coefs["ScenarioWrecking Crew"] + coefs["OneHeroTRUE"] + coefs["ScenarioWrecking Crew:OneHeroTRUE"],
    coefs["ScenarioRisky Business"] + coefs["OneHeroTRUE"] + coefs["ScenarioRisky Business:OneHeroTRUE"],
    coefs["ScenarioMutagen Formula"] + coefs["OneHeroTRUE"] + coefs["ScenarioMutagen Formula:OneHeroTRUE"],
    coefs["ScenarioCrossbones"] + coefs["OneHeroTRUE"] + coefs["ScenarioCrossbones:OneHeroTRUE"],
    coefs["ScenarioAbsorbing Man"] + coefs["OneHeroTRUE"] + coefs["ScenarioAbsorbing Man:OneHeroTRUE"],
    coefs["ScenarioTaskmaster"] + coefs["OneHeroTRUE"] + coefs["ScenarioTaskmaster:OneHeroTRUE"],
    coefs["ScenarioZola"] + coefs["OneHeroTRUE"] + coefs["ScenarioZola:OneHeroTRUE"],
    coefs["ScenarioRed Skull"] + coefs["OneHeroTRUE"] + coefs["ScenarioRed Skull:OneHeroTRUE"],
    coefs["ScenarioKang"] + coefs["OneHeroTRUE"] + coefs["ScenarioKang:OneHeroTRUE"],
    coefs["ScenarioBrotherhood of Badoon"] + coefs["OneHeroTRUE"] + coefs["ScenarioBrotherhood of Badoon:OneHeroTRUE"],
    coefs["ScenarioInfiltrate the Museum"] + coefs["OneHeroTRUE"] + coefs["ScenarioInfiltrate the Museum:OneHeroTRUE"],
    coefs["ScenarioEscape the Museum"] + coefs["OneHeroTRUE"] + coefs["ScenarioEscape the Museum:OneHeroTRUE"],
    coefs["ScenarioNebula"] + coefs["OneHeroTRUE"] + coefs["ScenarioNebula:OneHeroTRUE"],
    coefs["ScenarioRonan the Accuser"] + coefs["OneHeroTRUE"] + coefs["ScenarioRonan the Accuser:OneHeroTRUE"],
    coefs["ScenarioEbony Maw"] + coefs["OneHeroTRUE"] + coefs["ScenarioEbony Maw:OneHeroTRUE"],
    coefs["ScenarioTower Defense"] + coefs["OneHeroTRUE"] + coefs["ScenarioTower Defense:OneHeroTRUE"],
    coefs["ScenarioThanos"] + coefs["OneHeroTRUE"] + coefs["ScenarioThanos:OneHeroTRUE"],
    coefs["ScenarioHela"] + coefs["OneHeroTRUE"] + coefs["ScenarioHela:OneHeroTRUE"],
    coefs["ScenarioLoki"] + coefs["OneHeroTRUE"] + coefs["ScenarioLoki:OneHeroTRUE"],
    coefs["ScenarioThe Hood"] + coefs["OneHeroTRUE"] + coefs["ScenarioThe Hood:OneHeroTRUE"],
    coefs["ScenarioSandman"] + coefs["OneHeroTRUE"] + coefs["ScenarioSandman:OneHeroTRUE"],
    coefs["ScenarioVenom"] + coefs["OneHeroTRUE"] + coefs["ScenarioVenom:OneHeroTRUE"],
    coefs["ScenarioMysterio"] + coefs["OneHeroTRUE"] + coefs["ScenarioMysterio:OneHeroTRUE"],
    coefs["ScenarioThe Sinister Six"] + coefs["OneHeroTRUE"] + coefs["ScenarioThe Sinister Six:OneHeroTRUE"],
    coefs["ScenarioVenom Goblin"] + coefs["OneHeroTRUE"] + coefs["ScenarioVenom Goblin:OneHeroTRUE"],
    coefs["ScenarioSabretooth"] + coefs["OneHeroTRUE"] + coefs["ScenarioSabretooth:OneHeroTRUE"],
    coefs["ScenarioProject Wideawake"] + coefs["OneHeroTRUE"] + coefs["ScenarioProject Wideawake:OneHeroTRUE"],
    coefs["ScenarioMaster Mold"] + coefs["OneHeroTRUE"] + coefs["ScenarioMaster Mold:OneHeroTRUE"],
    coefs["ScenarioMansion Attack"] + coefs["OneHeroTRUE"] + coefs["ScenarioMansion Attack:OneHeroTRUE"],
    coefs["ScenarioMagneto"] + coefs["OneHeroTRUE"] + coefs["ScenarioMagneto:OneHeroTRUE"],
    coefs["ScenarioMaGog"] + coefs["OneHeroTRUE"] + coefs["ScenarioMaGog:OneHeroTRUE"],
    coefs["ScenarioSpiral"] + coefs["OneHeroTRUE"] + coefs["ScenarioSpiral:OneHeroTRUE"],
    coefs["ScenarioMojo"] + coefs["OneHeroTRUE"] + coefs["ScenarioMojo:OneHeroTRUE"],
    coefs["ScenarioMorlock Siege"] + coefs["OneHeroTRUE"] + coefs["ScenarioMorlock Siege:OneHeroTRUE"],
    coefs["ScenarioOn the Run"] + coefs["OneHeroTRUE"] + coefs["ScenarioOn the Run:OneHeroTRUE"],
    coefs["ScenarioJuggernaut"] + coefs["OneHeroTRUE"] + coefs["ScenarioJuggernaut:OneHeroTRUE"],
    coefs["ScenarioMister Sinister"] + coefs["OneHeroTRUE"] + coefs["ScenarioMister Sinister:OneHeroTRUE"],
    coefs["ScenarioStryfe"] + coefs["OneHeroTRUE"] + coefs["ScenarioStryfe:OneHeroTRUE"],
    coefs["ScenarioUnus"] + coefs["OneHeroTRUE"] + coefs["ScenarioUnus:OneHeroTRUE"],
    coefs["ScenarioFour Horsemen"] + coefs["OneHeroTRUE"] + coefs["ScenarioFour Horsemen:OneHeroTRUE"],
    coefs["ScenarioApocalypse"] + coefs["OneHeroTRUE"] + coefs["ScenarioApocalypse:OneHeroTRUE"],
    coefs["ScenarioDark Beast"] + coefs["OneHeroTRUE"] + coefs["ScenarioDark Beast:OneHeroTRUE"],
    coefs["ScenarioEn Sabah Nur"] + coefs["OneHeroTRUE"] + coefs["ScenarioEn Sabah Nur:OneHeroTRUE"]
  )

  return(
    data.frame(
      row.names = c(
        "Rhino",
        "Klaw",
        "Ultron",
        "Wrecking Crew",
        "Risky Business",
        "Mutagen Formula",
        "Crossbones",
        "Absorbing Man",
        "Taskmaster",
        "Zola",
        "Red Skull",
        "Kang",
        "Brotherhood of Badoon",
        "Infiltrate the Museum",
        "Escape the Museum",
        "Nebula",
        "Ronan the Accuser",
        "Ebony Maw",
        "Tower Defense",
        "Thanos",
        "Hela",
        "Loki",
        "The Hood",
        "Sandman",
        "Venom",
        "Mysterio",
        "The Sinister Six",
        "Venom Goblin",
        "Sabretooth",
        "Project Wideawake",
        "Master Mold",
        "Mansion Attack",
        "Magneto",
        "MaGog",
        "Spiral",
        "Mojo",
        "Morlock Siege",
        "On the Run",
        "Juggernaut",
        "Mister Sinister",
        "Stryfe",
        "Unus",
        "Four Horsemen",
        "Apocalypse",
        "Dark Beast",
        "En Sabah Nur"
      ),
      Multiplayer = round(multiplayer * 10),
      OneHero = round(oneHero * 10)
    )
  )
}
