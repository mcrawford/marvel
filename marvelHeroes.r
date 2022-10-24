MarvelHeroes <- function(marvelGlm) {
  coefs <- coef(marvelGlm)
  return(data.frame(
    row.names = c(
      "Adam Warlock",
      "Ant-Man",
      "Black Panther",
      "Black Widow",
      "Captain America",
      "Captain Marvel",
      "Colossus",
      "Cyclops",
      "Doctor Strange",
      "Drax",
      "Gamora",
      "Ghost-Spider",
      "Groot",
      "Hawkeye",
      "Hulk",
      "Iron Man",
      "Ironheart",
      "Ms. Marvel",
      "Nebula",
      "Nova",
      "Phoenix",
      "Quicksilver",
      "Rocket Raccoon",
      "Scarlet Witch",
      "Shadowcat",
      "She-Hulk",
      "Spdr",
      "Spectrum",
      "Spider-Ham",
      "Spider-Man",
      "Spider-Man (Miles Morales)",
      "Spider-Woman",
      "Star-Lord",
      "Thor",
      "Valkyrie",
      "Venom",
      "Vision",
      "War Machine",
      "Wasp"
    ),
    Ratings = c(
      sprintf("%+d", round(coefs["AdamWarlockTRUE"] * 10)),
      sprintf("%+d", round(coefs["AntManTRUE"] * 10)),
      sprintf("%+d", round(coefs["BlackPantherTRUE"] * 10)),
      sprintf("%+d", round(coefs["BlackWidowTRUE"] * 10)),
      sprintf("%+d", round(coefs["CaptainAmericaTRUE"] * 10)),
      sprintf("%+d", round(coefs["CaptainMarvelTRUE"] * 10)),
      sprintf("%+d", round(coefs["ColossusTRUE"] * 10)),
      sprintf("%+d", round(coefs["CyclopsTRUE"] * 10)),
      sprintf("%+d", round(coefs["DoctorStrangeTRUE"] * 10)),
      sprintf("%+d", round(coefs["DraxTRUE"] * 10)),
      sprintf("%+d", round(coefs["GamoraTRUE"] * 10)),
      sprintf("%+d", round(coefs["GhostSpiderTRUE"] * 10)),
      sprintf("%+d", round(coefs["GrootTRUE"] * 10)),
      sprintf("%+d", round(coefs["HawkeyeTRUE"] * 10)),
      sprintf("%+d", round(coefs["HulkTRUE"] * 10)),
      sprintf("%+d", round(coefs["IronManTRUE"] * 10)),
      sprintf("%+d", round(coefs["IronheartTRUE"] * 10)),
      sprintf("%+d", round(coefs["MsMarvelTRUE"] * 10)),
      sprintf("%+d", round(coefs["NebulaTRUE"] * 10)),
      sprintf("%+d", round(coefs["NovaTRUE"] * 10)),
      sprintf("%+d", round(coefs["PhoenixTRUE"] * 10)),
      sprintf("%+d", round(coefs["QuicksilverTRUE"] * 10)),
      sprintf("%+d", round(coefs["RocketRaccoonTRUE"] * 10)),
      sprintf("%+d", round(coefs["ScarletWitchTRUE"] * 10)),
      sprintf("%+d", round(coefs["ShadowcatTRUE"] * 10)),
      sprintf("%+d", round(coefs["SheHulkTRUE"] * 10)),
      sprintf("%+d", round(coefs["SpdrTRUE"] * 10)),
      sprintf("%+d", round(coefs["SpectrumTRUE"] * 10)),
      sprintf("%+d", round(coefs["SpiderHamTRUE"] * 10)),
      sprintf("%+d", round(coefs["SpiderManTRUE"] * 10)),
      sprintf("%+d", round(coefs["SpiderManMilesMoralesTRUE"] * 10)),
      sprintf("%+d", round(coefs["SpiderWomanTRUE"] * 10)),
      sprintf("%+d", round(coefs["StarLordTRUE"] * 10)),
      sprintf("%+d", round(coefs["ThorTRUE"] * 10)),
      sprintf("%+d", round(coefs["ValkyrieTRUE"] * 10)),
      sprintf("%+d", round(coefs["VenomTRUE"] * 10)),
      sprintf("%+d", round(coefs["VisionTRUE"] * 10)),
      sprintf("%+d", round(coefs["WarMachineTRUE"] * 10)),
      sprintf("%+d", round(coefs["WaspTRUE"] * 10))
    )
  ))
}