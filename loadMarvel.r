library(anytime)

LoadMarvel <- function(filename) {
  marvel           <- read.csv(filename)
  colnames(marvel) <- c(
    "Timestamp", "First", "FirstAspect", "IsSecondHero", "FirstTwoAspects", "IsSecondHero1", "IsSecondHero2",
    "Second", "SecondAspect", "IsThirdHero", "SecondTwoAspects", "IsThirdHero1", "IsThirdHero2",
    "Third", "ThirdAspect", "IsFourthHero", "ThirdTwoAspects", "IsFourthHero1", "IsFourthHero2",
    "Fourth", "FourthAspect", "FourthTwoAspects",
    "Scenario", "Campaign", "ExpertCampaign", "Encounter",
    "Win", "Standard", "Expert", "Heroic", "Skirmish",
    "WinWC", "DifficultyWC", "HeroicWC", "Difficulty"
  )

  marvel$Timestamp                    <- anytime(marvel$Timestamp)
  marvel$Scenario                     <- relevel(factor(marvel$Scenario), ref = "Rhino")
  marvel$Win[marvel$Win == ""]        <- marvel$WinWC[marvel$Win == ""]
  marvel$Heroic[is.na(marvel$Heroic)] <- marvel$HeroicWC[is.na(marvel$Heroic)]

  meanDate <- mean(marvel$Timestamp)
  for (i in 1:4) {
    for (s in levels(marvel$Scenario)) {
      multiStandard <- data.frame(
        Timestamp = meanDate,
        First     = "", FirstAspect = "", IsSecondHero = "Yes", FirstTwoAspects = "", IsSecondHero1 = "No", IsSecondHero2 = "No",
        Second    = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
        Third     = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
        Fourth    = "", FourthAspect = "", FourthTwoAspects = "",
        Scenario  = s,
        Campaign  = "No", ExpertCampaign = "No", Encounter = "",
        Win       = if (i <= 3) "Yes" else "No", Standard = "Standard (Core Set)", Expert = "", Heroic = 0, Skirmish = 0,
        WinWC     = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Standard"
      )
      multiExpert   <- data.frame(
        Timestamp = meanDate,
        First     = "", FirstAspect = "", IsSecondHero = "Yes", FirstTwoAspects = "", IsSecondHero1 = "No", IsSecondHero2 = "No",
        Second    = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
        Third     = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
        Fourth    = "", FourthAspect = "", FourthTwoAspects = "",
        Scenario  = s,
        Campaign  = "No", ExpertCampaign = "No", Encounter = "",
        Win       = if (i <= 2) "Yes" else "No", Standard = "Standard (Core Set)", Expert = "Expert (Core Set)", Heroic = 0, Skirmish = 0,
        WinWC     = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Expert"
      )
      soloStandard  <- data.frame(
        Timestamp = meanDate,
        First     = "", FirstAspect = "", IsSecondHero = "No", FirstTwoAspects = "", IsSecondHero1 = "No", IsSecondHero2 = "No",
        Second    = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
        Third     = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
        Fourth    = "", FourthAspect = "", FourthTwoAspects = "",
        Scenario  = s,
        Campaign  = "No", ExpertCampaign = "No", Encounter = "",
        Win       = if (i <= 3) "Yes" else "No", Standard = "Standard (Core Set)", Expert = "", Heroic = 0, Skirmish = 0,
        WinWC     = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Standard"
      )
      soloExpert    <- data.frame(
        Timestamp = meanDate,
        First     = "", FirstAspect = "", IsSecondHero = "No", FirstTwoAspects = "", IsSecondHero1 = "No", IsSecondHero2 = "No",
        Second    = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
        Third     = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
        Fourth    = "", FourthAspect = "", FourthTwoAspects = "",
        Scenario  = s,
        Campaign  = "No", ExpertCampaign = "No", Encounter = "",
        Win       = if (i <= 2) "Yes" else "No", Standard = "Standard (Core Set)", Expert = "Expert (Core Set)", Heroic = 0, Skirmish = 0,
        WinWC     = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Expert"
      )
      marvel <- rbind(marvel, multiStandard)
      marvel <- rbind(marvel, multiExpert)
      marvel <- rbind(marvel, soloStandard)
      marvel <- rbind(marvel, soloExpert)
    }
  }
  for (i in 1:3) {
    for (es in GetEncounterSets()) {
      dummyEncounterSet <- data.frame(
        Timestamp = meanDate,
        First     = "", FirstAspect = "", IsSecondHero = "Yes", FirstTwoAspects = "", IsSecondHero1 = "No", IsSecondHero2 = "No",
        Second    = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
        Third     = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
        Fourth    = "", FourthAspect = "", FourthTwoAspects = "",
        Scenario  = "",
        Campaign  = "No", ExpertCampaign = "No", Encounter = es,
        Win       = if (i <= 2) "Yes" else "No", Standard = "Standard (Core Set)", Expert = "", Heroic = 0, Skirmish = 0,
        WinWC     = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Standard"
      )
      marvel <- rbind(marvel, dummyEncounterSet)
    }
  }

  marvel$DifficultyLevel <- "Standard"
  marvel$DifficultyLevel[grepl("Standard", marvel$Standard, fixed = TRUE)]     <- "Standard"
  marvel$DifficultyLevel[grepl("Expert", marvel$Expert, fixed = TRUE)]         <- "Expert"
  marvel$DifficultyLevel[grepl("Standard", marvel$DifficultyWC, fixed = TRUE)] <- "Standard"
  marvel$DifficultyLevel[grepl("Expert", marvel$DifficultyWC, fixed = TRUE)]   <- "Expert"
  marvel$DifficultyLevel[grepl("Extreme", marvel$DifficultyWC, fixed = TRUE)]  <- "Expert"
  marvel$DifficultyLevel <- relevel(factor(marvel$DifficultyLevel), ref = "Standard")

  marvel$Standard2 <- (marvel$Standard == "Standard II (The Hood scenario pack)")
  marvel$Expert2   <- (marvel$Expert == "Expert II (The Hood scenario pack)")

  marvel$OneHero                                 <- (marvel$IsSecondHero == "No")
  marvel$OneHero[marvel$First == "Spider-Woman"] <- (marvel$IsSecondHero1[marvel$First == "Spider-Woman"] == "No")
  marvel$OneHero[marvel$First == "Adam Warlock"] <- (marvel$IsSecondHero2[marvel$First == "Adam Warlock"] == "No")

  marvel$Heroic[is.na(marvel$Heroic)]     <- 0
  marvel$Heroic[marvel$Heroic > 3]        <- 3
  marvel$Skirmish[is.na(marvel$Skirmish)] <- 0
  marvel$Skirmish[marvel$Skirmish > 3]    <- 3
  marvel$SkirmishLevel                    <- factor(marvel$Skirmish)
  marvel$Skirmish                         <- (marvel$Skirmish > 0)

  marvel$Campaign                 <- (marvel$Campaign == "Yes")
  marvel$CampaignAbsorbingMan     <- (marvel$Campaign & marvel$Scenario == "Absorbing Man")
  marvel$CampaignTaskmaster       <- (marvel$Campaign & marvel$Scenario == "Taskmaster")
  marvel$CampaignZola             <- (marvel$Campaign & marvel$Scenario == "Zola")
  marvel$CampaignRedSkull         <- (marvel$Campaign & marvel$Scenario == "Red Skull")
  marvel$CampaignBrotherhood      <- (marvel$Campaign & marvel$Scenario == "Brotherhood of Badoon")
  marvel$CampaignInfiltrateMuseum <- (marvel$Campaign & marvel$Scenario == "Infiltrate the Museum")
  marvel$CampaignEscapeMuseum     <- (marvel$Campaign & marvel$Scenario == "Escape the Museum")
  marvel$CampaignNebula           <- (marvel$Campaign & marvel$Scenario == "Nebula")
  marvel$CampaignRonan            <- (marvel$Campaign & marvel$Scenario == "Ronan the Accuser")
  marvel$CampaignEbonyMaw         <- (marvel$Campaign & marvel$Scenario == "Ebony Maw")
  marvel$CampaignTowerDefense     <- (marvel$Campaign & marvel$Scenario == "Tower Defense")
  marvel$CampaignThanos           <- (marvel$Campaign & marvel$Scenario == "Thanos")
  marvel$CampaignHela             <- (marvel$Campaign & marvel$Scenario == "Hela")
  marvel$CampaignLoki             <- (marvel$Campaign & marvel$Scenario == "Loki")
  marvel$CampaignSandman          <- (marvel$Campaign & marvel$Scenario == "Sandman")
  marvel$CampaignVenom            <- (marvel$Campaign & marvel$Scenario == "Venom")
  marvel$CampaignMysterio         <- (marvel$Campaign & marvel$Scenario == "Mysterio")
  marvel$CampaignSinisterSix      <- (marvel$Campaign & marvel$Scenario == "The Sinister Six")
  marvel$CampaignVenomGoblin      <- (marvel$Campaign & marvel$Scenario == "Venom Goblin")
  marvel$CampaignSabretooth       <- (marvel$Campaign & marvel$Scenario == "Sabretooth")
  marvel$CampaignProjectWideawake <- (marvel$Campaign & marvel$Scenario == "Project Wideawake")
  marvel$CampaignMasterMold       <- (marvel$Campaign & marvel$Scenario == "Master Mold")
  marvel$CampaignMansionAttack    <- (marvel$Campaign & marvel$Scenario == "Mansion Attack")
  marvel$CampaignMagneto          <- (marvel$Campaign & marvel$Scenario == "Magneto")
  marvel$CampaignMorlockSiege     <- (marvel$Campaign & marvel$Scenario == "Morlock Siege")
  marvel$CampaignOnTheRun         <- (marvel$Campaign & marvel$Scenario == "On the Run")
  marvel$CampaignJuggernaut       <- (marvel$Campaign & marvel$Scenario == "Juggernaut")
  marvel$CampaignMisterSinister   <- (marvel$Campaign & marvel$Scenario == "Mister Sinister")
  marvel$CampaignStryfe           <- (marvel$Campaign & marvel$Scenario == "Stryfe")

  marvel$Undeclared          <- marvel$Encounter == "" & marvel$Scenario != "Wrecking Crew"
  marvel$BombScare           <- grepl("Bomb Scare", marvel$Encounter, fixed = TRUE)
  marvel$MastersOfEvil       <- grepl("Masters of Evil", marvel$Encounter, fixed = TRUE)
  marvel$UnderAttack         <- grepl("Under Attack", marvel$Encounter, fixed = TRUE)
  marvel$LegionsOfHydra      <- grepl("Legions of Hydra", marvel$Encounter, fixed = TRUE)
  marvel$DoomsdayChair       <- grepl("The Doomsday Chair", marvel$Encounter, fixed = TRUE)
  marvel$GoblinGimmicks      <- grepl("Goblin Gimmicks", marvel$Encounter, fixed = TRUE)
  marvel$MessOfThings        <- grepl("A Mess of Things", marvel$Encounter, fixed = TRUE)
  marvel$PowerDrain          <- grepl("Power Drain", marvel$Encounter, fixed = TRUE)
  marvel$RunningInterference <- grepl("Running Interference", marvel$Encounter, fixed = TRUE)
  marvel$KreeFanatic         <- grepl("Kree Fanatic", marvel$Encounter, fixed = TRUE)
  marvel$HydraAssault        <- grepl("Hydra Assault", marvel$Encounter, fixed = TRUE)
  marvel$HydraPatrol         <- grepl("Hydra Patrol", marvel$Encounter, fixed = TRUE)
  marvel$WeaponMaster        <- grepl("Weapon Master", marvel$Encounter, fixed = TRUE)
  marvel$ExperimentalWeapons <- grepl("Experimental Weapons", marvel$Encounter, fixed = TRUE)
  marvel$Temporal            <- grepl("Temporal", marvel$Encounter, fixed = TRUE)
  marvel$MasterOfTime        <- grepl("Master of Time", marvel$Encounter, fixed = TRUE)
  marvel$Anachronauts        <- grepl("Anachronauts", marvel$Encounter, fixed = TRUE)
  marvel$BandOfBadoon        <- grepl("Band of Badoon", marvel$Encounter, fixed = TRUE)
  marvel$MenagerieMedley     <- grepl("Menagerie Medley", marvel$Encounter, fixed = TRUE)
  marvel$GalacticArtifacts   <- grepl("Galactic Artifacts", marvel$Encounter, fixed = TRUE)
  marvel$SpacePirates        <- grepl("Space Pirates", marvel$Encounter, fixed = TRUE)
  marvel$KreeMilitants       <- grepl("Kree Militants", marvel$Encounter, fixed = TRUE)
  marvel$ShipCommand         <- grepl("Ship Command", marvel$Encounter, fixed = TRUE)
  marvel$BadoonHeadhunter    <- grepl("Badoon Headhunter", marvel$Encounter, fixed = TRUE)
  marvel$PowerStone          <- grepl("Power Stone", marvel$Encounter, fixed = TRUE)
  marvel$BlackOrder          <- grepl("Black Order", marvel$Encounter, fixed = TRUE)
  marvel$ArmiesOfTitan       <- grepl("Armies of Titan", marvel$Encounter, fixed = TRUE)
  marvel$ChildrenOfThanos    <- grepl("Children of Thanos", marvel$Encounter, fixed = TRUE)
  marvel$LegionsOfHel        <- grepl("Legions of Hel", marvel$Encounter, fixed = TRUE)
  marvel$FrostGiants         <- grepl("Frost Giants", marvel$Encounter, fixed = TRUE)
  marvel$Enchantress         <- grepl("Enchantress", marvel$Encounter, fixed = TRUE)
  marvel$InfinityGauntlet    <- grepl("Infinity Gauntlet", marvel$Encounter, fixed = TRUE)
  marvel$BeastyBoys          <- grepl("Beasty Boys", marvel$Encounter, fixed = TRUE)
  marvel$BrothersGrimm       <- grepl("Brothers Grimm", marvel$Encounter, fixed = TRUE)
  marvel$CrossfiresCrew      <- grepl("Crossfire's Crew", marvel$Encounter, fixed = TRUE)
  marvel$MisterHyde          <- grepl("Mister Hyde", marvel$Encounter, fixed = TRUE)
  marvel$RansackedArmory     <- grepl("Ransacked Armory", marvel$Encounter, fixed = TRUE)
  marvel$SinisterSyndicate   <- grepl("Sinister Syndicate", marvel$Encounter, fixed = TRUE)
  marvel$StateOfEmergency    <- grepl("State of Emergency", marvel$Encounter, fixed = TRUE)
  marvel$StreetsOfMayhem     <- grepl("Streets of Mayhem", marvel$Encounter, fixed = TRUE)
  marvel$WreckingCrew        <- grepl("Wrecking Crew", marvel$Encounter, fixed = TRUE)
  marvel$CityInChaos         <- grepl("City in Chaos", marvel$Encounter, fixed = TRUE)
  marvel$DownToEarth         <- grepl("Down to Earth", marvel$Encounter, fixed = TRUE)
  marvel$GoblinGear          <- grepl("Goblin Gear", marvel$Encounter, fixed = TRUE)
  marvel$GuerrillaTactics    <- grepl("Guerrilla Tactics", marvel$Encounter, fixed = TRUE)
  marvel$OsbornTech          <- grepl("Osborn Tech", marvel$Encounter, fixed = TRUE)
  marvel$PersonalNightmare   <- grepl("Personal Nightmare", marvel$Encounter, fixed = TRUE)
  marvel$SinisterAssault     <- grepl("Sinister Assault", marvel$Encounter, fixed = TRUE)
  marvel$SymbioticStrength   <- grepl("Symbiotic Strength", marvel$Encounter, fixed = TRUE)
  marvel$WhispersOfParanoia  <- grepl("Whispers of Paranoia", marvel$Encounter, fixed = TRUE)
  marvel$Armadillo           <- grepl("Armadillo", marvel$Encounter, fixed = TRUE)
  marvel$Zzzax               <- grepl("Zzzax", marvel$Encounter, fixed = TRUE)
  marvel$Inheritors          <- grepl("The Inheritors", marvel$Encounter, fixed = TRUE)
  marvel$ISSinisterSix       <- grepl("Iron Spider's Sinister Six", marvel$Encounter, fixed = TRUE)
  marvel$Deathstrike         <- grepl("Deathstrike", marvel$Encounter, fixed = TRUE)
  marvel$ShadowKing          <- grepl("Shadow King", marvel$Encounter, fixed = TRUE)
  marvel$Mystique            <- grepl("Mystique", marvel$Encounter, fixed = TRUE)
  marvel$Brotherhood         <- grepl("Brotherhood", marvel$Encounter, fixed = TRUE)
  marvel$ZeroTolerance       <- grepl("Zero Tolerance", marvel$Encounter, fixed = TRUE)
  marvel$Sentinels           <- grepl("Sentinels", marvel$Encounter, fixed = TRUE)
  marvel$Acolytes            <- grepl("Acolytes", marvel$Encounter, fixed = TRUE)
  marvel$FuturePast          <- grepl("Future Past", marvel$Encounter, fixed = TRUE)
  marvel$Crime               <- grepl("Crime", marvel$Encounter, fixed = TRUE)
  marvel$Fantasy             <- grepl("Fantasy", marvel$Encounter, fixed = TRUE)
  marvel$Horror              <- grepl("Horror", marvel$Encounter, fixed = TRUE)
  marvel$SciFi               <- grepl("Sci-Fi", marvel$Encounter, fixed = TRUE)
  marvel$Sitcom              <- grepl("Sitcom", marvel$Encounter, fixed = TRUE)
  marvel$Western             <- grepl("Western", marvel$Encounter, fixed = TRUE)
  marvel$Longshot            <- grepl("Longshot", marvel$Encounter, fixed = TRUE)
  marvel$BlackTomCassidy     <- grepl("Black Tom Cassidy", marvel$Encounter, fixed = TRUE)
  marvel$Exodus              <- grepl("Exodus", marvel$Encounter, fixed = TRUE)
  marvel$ExtremeMeasures     <- grepl("Extreme Measures", marvel$Encounter, fixed = TRUE)
  marvel$Flight              <- grepl("Flight", marvel$Encounter, fixed = TRUE)
  marvel$MilitaryGrade       <- grepl("Military Grade", marvel$Encounter, fixed = TRUE)
  marvel$MutantInsurrection  <- grepl("Mutant Insurrection", marvel$Encounter, fixed = TRUE)
  marvel$MutantSlayers       <- grepl("Mutant Slayers", marvel$Encounter, fixed = TRUE)
  marvel$NastyBoys           <- grepl("Nasty Boys", marvel$Encounter, fixed = TRUE)
  marvel$Reavers             <- grepl("Reavers", marvel$Encounter, fixed = TRUE)
  marvel$SuperStrength       <- grepl("Super Strength", marvel$Encounter, fixed = TRUE)
  marvel$Telepathy           <- grepl("Telepathy", marvel$Encounter, fixed = TRUE)
  marvel$HopeSummers         <- grepl("Hope Summers", marvel$Encounter, fixed = TRUE)

  marvel$HydraPatrol[marvel$Scenario == "Taskmaster"]                  <- TRUE
  marvel$GalacticArtifacts[marvel$Scenario == "Infiltrate the Museum"] <- TRUE
  marvel$GalacticArtifacts[marvel$Scenario == "Escape the Museum"]     <- TRUE
  marvel$ShipCommand[marvel$Scenario == "Brotherhood of Badoon"]       <- TRUE
  marvel$ShipCommand[marvel$Scenario == "Nebula"]                      <- TRUE
  marvel$ShipCommand[marvel$Scenario == "Ronan the Accuser"]           <- TRUE
  marvel$PowerStone[marvel$Scenario == "Nebula"]                       <- TRUE
  marvel$PowerStone[marvel$Scenario == "Ronan the Accuser"]            <- TRUE
  marvel$InfinityGauntlet[marvel$Scenario == "Thanos"]                 <- TRUE
  marvel$InfinityGauntlet[marvel$Scenario == "Loki"]                   <- TRUE
  marvel$CityInChaos[marvel$Scenario == "Sandman"]                     <- TRUE
  marvel$SymbioticStrength[marvel$Scenario == "Venom"]                 <- TRUE
  marvel$PersonalNightmare[marvel$Scenario == "Mysterio"]              <- TRUE
  marvel$SymbioticStrength[marvel$Scenario == "Venom Goblin"]          <- TRUE
  marvel$ZeroTolerance[marvel$Scenario == "Project Wideawake"]         <- TRUE
  marvel$Sentinels[marvel$Scenario == "Master Mold"]                   <- TRUE
  marvel$Brotherhood[marvel$Scenario == "Mansion Attack"]              <- TRUE
  marvel$MutantSlayers[marvel$Scenario == "On the Run"]                <- TRUE
  marvel$HopeSummers[marvel$Scenario == "Juggernaut"]                  <- TRUE
  marvel$HopeSummers[marvel$Scenario == "Mister Sinister"]             <- TRUE
  marvel$HopeSummers[marvel$Scenario == "Stryfe"]                      <- TRUE

  marvel$Aggression <- marvel$FirstAspect == "Aggression" |
    marvel$SecondAspect == "Aggression" |
    marvel$ThirdAspect == "Aggression" |
    marvel$FourthAspect == "Aggression"
  marvel$Justice    <- marvel$FirstAspect == "Justice" |
    marvel$SecondAspect == "Justice" |
    marvel$ThirdAspect == "Justice" |
    marvel$FourthAspect == "Justice"
  marvel$Leadership <- marvel$FirstAspect == "Leadership" |
    marvel$SecondAspect == "Leadership" |
    marvel$ThirdAspect == "Leadership" |
    marvel$FourthAspect == "Leadership"
  marvel$Protection <- marvel$FirstAspect == "Protection" |
    marvel$SecondAspect == "Protection" |
    marvel$ThirdAspect == "Protection" |
    marvel$FourthAspect == "Protection"
  marvel$Pool <- marvel$FirstAspect == "Pool" |
    marvel$SecondAspect == "Pool" |
    marvel$ThirdAspect == "Pool" |
    marvel$FourthAspect == "Pool"

  marvel$AdamWarlock <- marvel$First == "Adam Warlock" |
    marvel$Second == "Adam Warlock" |
    marvel$Third == "Adam Warlock" |
    marvel$Fourth == "Adam Warlock"
  marvel$Angel <- marvel$First == "Angel" |
    marvel$Second == "Angel" |
    marvel$Third == "Angel" |
    marvel$Fourth == "Angel"
  marvel$AntMan <- marvel$First == "Ant-Man" |
    marvel$Second == "Ant-Man" |
    marvel$Third == "Ant-Man" |
    marvel$Fourth == "Ant-Man"
  marvel$BlackPanther <- marvel$First == "Black Panther" |
    marvel$Second == "Black Panther" |
    marvel$Third == "Black Panther" |
    marvel$Fourth == "Black Panther"
  marvel$BlackWidow <- marvel$First == "Black Widow" |
    marvel$Second == "Black Widow" |
    marvel$Third == "Black Widow" |
    marvel$Fourth == "Black Widow"
  marvel$Cable <- marvel$First == "Cable" |
    marvel$Second == "Cable" |
    marvel$Third == "Cable" |
    marvel$Fourth == "Cable"
  marvel$CaptainAmerica <- marvel$First == "Captain America" |
    marvel$Second == "Captain America" |
    marvel$Third == "Captain America" |
    marvel$Fourth == "Captain America"
  marvel$CaptainMarvel <- marvel$First == "Captain Marvel" |
    marvel$Second == "Captain Marvel" |
    marvel$Third == "Captain Marvel" |
    marvel$Fourth == "Captain Marvel"
  marvel$Colossus <- marvel$First == "Colossus" |
    marvel$Second == "Colossus" |
    marvel$Third == "Colossus" |
    marvel$Fourth == "Colossus"
  marvel$Cyclops <- marvel$First == "Cyclops" |
    marvel$Second == "Cyclops" |
    marvel$Third == "Cyclops" |
    marvel$Fourth == "Cyclops"
  marvel$Deadpool <- marvel$First == "Deadpool" |
    marvel$Second == "Deadpool" |
    marvel$Third == "Deadpool" |
    marvel$Fourth == "Deadpool"
  marvel$DoctorStrange <- marvel$First == "Doctor Strange" |
    marvel$Second == "Doctor Strange" |
    marvel$Third == "Doctor Strange" |
    marvel$Fourth == "Doctor Strange"
  marvel$Domino <- marvel$First == "Domino" |
    marvel$Second == "Domino" |
    marvel$Third == "Domino" |
    marvel$Fourth == "Domino"
  marvel$Drax <- marvel$First == "Drax" |
    marvel$Second == "Drax" |
    marvel$Third == "Drax" |
    marvel$Fourth == "Drax"
  marvel$Gambit <- marvel$First == "Gambit" |
    marvel$Second == "Gambit" |
    marvel$Third == "Gambit" |
    marvel$Fourth == "Gambit"
  marvel$Gamora <- marvel$First == "Gamora" |
    marvel$Second == "Gamora" |
    marvel$Third == "Gamora" |
    marvel$Fourth == "Gamora"
  marvel$GhostSpider <- marvel$First == "Ghost-Spider" |
    marvel$Second == "Ghost-Spider" |
    marvel$Third == "Ghost-Spider" |
    marvel$Fourth == "Ghost-Spider"
  marvel$Groot <- marvel$First == "Groot" |
    marvel$Second == "Groot" |
    marvel$Third == "Groot" |
    marvel$Fourth == "Groot"
  marvel$Hawkeye <- marvel$First == "Hawkeye" |
    marvel$Second == "Hawkeye" |
    marvel$Third == "Hawkeye" |
    marvel$Fourth == "Hawkeye"
  marvel$Hulk <- marvel$First == "Hulk" |
    marvel$Second == "Hulk" |
    marvel$Third == "Hulk" |
    marvel$Fourth == "Hulk"
  marvel$IronMan <- marvel$First == "Iron Man" |
    marvel$Second == "Iron Man" |
    marvel$Third == "Iron Man" |
    marvel$Fourth == "Iron Man"
  marvel$Ironheart <- marvel$First == "Ironheart" |
    marvel$Second == "Ironheart" |
    marvel$Third == "Ironheart" |
    marvel$Fourth == "Ironheart"
  marvel$MsMarvel <- marvel$First == "Ms. Marvel" |
    marvel$Second == "Ms. Marvel" |
    marvel$Third == "Ms. Marvel" |
    marvel$Fourth == "Ms. Marvel"
  marvel$Nebula <- marvel$First == "Nebula" |
    marvel$Second == "Nebula" |
    marvel$Third == "Nebula" |
    marvel$Fourth == "Nebula"
  marvel$Nova <- marvel$First == "Nova" |
    marvel$Second == "Nova" |
    marvel$Third == "Nova" |
    marvel$Fourth == "Nova"
  marvel$Phoenix <- marvel$First == "Phoenix" |
    marvel$Second == "Phoenix" |
    marvel$Third == "Phoenix" |
    marvel$Fourth == "Phoenix"
  marvel$Psylocke <- marvel$First == "Psylocke" |
    marvel$Second == "Psylocke" |
    marvel$Third == "Psylocke" |
    marvel$Fourth == "Psylocke"
  marvel$Quicksilver <- marvel$First == "Quicksilver" |
    marvel$Second == "Quicksilver" |
    marvel$Third == "Quicksilver" |
    marvel$Fourth == "Quicksilver"
  marvel$RocketRaccoon <- marvel$First == "Rocket Raccoon" |
    marvel$Second == "Rocket Raccoon" |
    marvel$Third == "Rocket Raccoon" |
    marvel$Fourth == "Rocket Raccoon"
  marvel$Rogue <- marvel$First == "Rogue" |
    marvel$Second == "Rogue" |
    marvel$Third == "Rogue" |
    marvel$Fourth == "Rogue"
  marvel$ScarletWitch <- marvel$First == "Scarlet Witch" |
    marvel$Second == "Scarlet Witch" |
    marvel$Third == "Scarlet Witch" |
    marvel$Fourth == "Scarlet Witch"
  marvel$Shadowcat <- marvel$First == "Shadowcat" |
    marvel$Second == "Shadowcat" |
    marvel$Third == "Shadowcat" |
    marvel$Fourth == "Shadowcat"
  marvel$SheHulk <- marvel$First == "She-Hulk" |
    marvel$Second == "She-Hulk" |
    marvel$Third == "She-Hulk" |
    marvel$Fourth == "She-Hulk"
  marvel$Spdr <- marvel$First == "SP//dr" |
    marvel$Second == "SP//dr" |
    marvel$Third == "SP//dr" |
    marvel$Fourth == "SP//dr"
  marvel$Spectrum <- marvel$First == "Spectrum" |
    marvel$Second == "Spectrum" |
    marvel$Third == "Spectrum" |
    marvel$Fourth == "Spectrum"
  marvel$SpiderHam <- marvel$First == "Spider-Ham" |
    marvel$Second == "Spider-Ham" |
    marvel$Third == "Spider-Ham" |
    marvel$Fourth == "Spider-Ham"
  marvel$SpiderMan <- marvel$First == "Spider-Man (Peter Parker)" |
    marvel$Second == "Spider-Man (Peter Parker)" |
    marvel$Third == "Spider-Man (Peter Parker)" |
    marvel$Fourth == "Spider-Man (Peter Parker)"
  marvel$SpiderManMilesMorales <- marvel$First == "Spider-Man (Miles Morales)" |
    marvel$Second == "Spider-Man (Miles Morales)" |
    marvel$Third == "Spider-Man (Miles Morales)" |
    marvel$Fourth == "Spider-Man (Miles Morales)"
  marvel$SpiderWoman <- marvel$First == "Spider-Woman" |
    marvel$Second == "Spider-Woman" |
    marvel$Third == "Spider-Woman" |
    marvel$Fourth == "Spider-Woman"
  marvel$StarLord <- marvel$First == "Star-Lord" |
    marvel$Second == "Star-Lord" |
    marvel$Third == "Star-Lord" |
    marvel$Fourth == "Star-Lord"
  marvel$Storm <- marvel$First == "Storm" |
    marvel$Second == "Storm" |
    marvel$Third == "Storm" |
    marvel$Fourth == "Storm"
  marvel$Thor <- marvel$First == "Thor" |
    marvel$Second == "Thor" |
    marvel$Third == "Thor" |
    marvel$Fourth == "Thor"
  marvel$Valkyrie <- marvel$First == "Valkyrie" |
    marvel$Second == "Valkyrie" |
    marvel$Third == "Valkyrie" |
    marvel$Fourth == "Valkyrie"
  marvel$Venom <- marvel$First == "Venom" |
    marvel$Second == "Venom" |
    marvel$Third == "Venom" |
    marvel$Fourth == "Venom"
  marvel$Vision <- marvel$First == "Vision" |
    marvel$Second == "Vision" |
    marvel$Third == "Vision" |
    marvel$Fourth == "Vision"
  marvel$WarMachine <- marvel$First == "War Machine" |
    marvel$Second == "War Machine" |
    marvel$Third == "War Machine" |
    marvel$Fourth == "War Machine"
  marvel$Wasp <- marvel$First == "Wasp" |
    marvel$Second == "Wasp" |
    marvel$Third == "Wasp" |
    marvel$Fourth == "Wasp"
  marvel$Wolverine <- marvel$First == "Wolverine" |
    marvel$Second == "Wolverine" |
    marvel$Third == "Wolverine" |
    marvel$Fourth == "Wolverine"
  marvel$X23 <- marvel$First == "X-23" |
    marvel$Second == "X-23" |
    marvel$Third == "X-23" |
    marvel$Fourth == "X-23"

  marvel$AngelSolo                 <- marvel$Angel & marvel$OneHero
  marvel$AdamWarlockSolo           <- marvel$AdamWarlock & marvel$OneHero
  marvel$AntManSolo                <- marvel$AntMan & marvel$OneHero
  marvel$BlackPantherSolo          <- marvel$BlackPanther & marvel$OneHero
  marvel$BlackWidowSolo            <- marvel$BlackWidow & marvel$OneHero
  marvel$CableSolo                 <- marvel$Cable & marvel$OneHero
  marvel$CaptainAmericaSolo        <- marvel$CaptainAmerica & marvel$OneHero
  marvel$CaptainMarvelSolo         <- marvel$CaptainMarvel & marvel$OneHero
  marvel$ColossusSolo              <- marvel$Colossus & marvel$OneHero
  marvel$CyclopsSolo               <- marvel$Cyclops & marvel$OneHero
  marvel$DeadpoolSolo              <- marvel$Deadpool & marvel$OneHero
  marvel$DoctorStrangeSolo         <- marvel$DoctorStrange & marvel$OneHero
  marvel$DominoSolo                <- marvel$Domino & marvel$OneHero
  marvel$DraxSolo                  <- marvel$Drax & marvel$OneHero
  marvel$GambitSolo                <- marvel$Gambit & marvel$OneHero
  marvel$GamoraSolo                <- marvel$Gamora & marvel$OneHero
  marvel$GhostSpiderSolo           <- marvel$GhostSpider & marvel$OneHero
  marvel$GrootSolo                 <- marvel$Groot & marvel$OneHero
  marvel$HawkeyeSolo               <- marvel$Hawkeye & marvel$OneHero
  marvel$HulkSolo                  <- marvel$Hulk & marvel$OneHero
  marvel$IronManSolo               <- marvel$IronMan & marvel$OneHero
  marvel$IronheartSolo             <- marvel$Ironheart & marvel$OneHero
  marvel$MsMarvelSolo              <- marvel$MsMarvel & marvel$OneHero
  marvel$NebulaSolo                <- marvel$Nebula & marvel$OneHero
  marvel$NovaSolo                  <- marvel$Nova & marvel$OneHero
  marvel$PhoenixSolo               <- marvel$Phoenix & marvel$OneHero
  marvel$PsylockeSolo              <- marvel$Psylocke & marvel$OneHero
  marvel$RogueSolo                 <- marvel$Rogue & marvel$OneHero
  marvel$QuicksilverSolo           <- marvel$Quicksilver & marvel$OneHero
  marvel$RocketRaccoonSolo         <- marvel$RocketRaccoon & marvel$OneHero
  marvel$ScarletWitchSolo          <- marvel$ScarletWitch & marvel$OneHero
  marvel$ShadowcatSolo             <- marvel$Shadowcat & marvel$OneHero
  marvel$SheHulkSolo               <- marvel$SheHulk & marvel$OneHero
  marvel$SpdrSolo                  <- marvel$Spdr & marvel$OneHero
  marvel$SpectrumSolo              <- marvel$Spectrum & marvel$OneHero
  marvel$SpiderHamSolo             <- marvel$SpiderHam & marvel$OneHero
  marvel$SpiderManSolo             <- marvel$SpiderMan & marvel$OneHero
  marvel$SpiderManMilesMoralesSolo <- marvel$SpiderManMilesMorales & marvel$OneHero
  marvel$SpiderWomanSolo           <- marvel$SpiderWoman & marvel$OneHero
  marvel$StarLordSolo              <- marvel$StarLord & marvel$OneHero
  marvel$StormSolo                 <- marvel$Storm & marvel$OneHero
  marvel$ThorSolo                  <- marvel$Thor & marvel$OneHero
  marvel$ValkyrieSolo              <- marvel$Valkyrie & marvel$OneHero
  marvel$VenomSolo                 <- marvel$Venom & marvel$OneHero
  marvel$VisionSolo                <- marvel$Vision & marvel$OneHero
  marvel$WarMachineSolo            <- marvel$WarMachine & marvel$OneHero
  marvel$WaspSolo                  <- marvel$Wasp & marvel$OneHero
  marvel$WolverineSolo             <- marvel$Wolverine & marvel$OneHero
  marvel$X23Solo                   <- marvel$X23 & marvel$OneHero

  return(marvel)
}

GetEncounterSets <- function() {
  c(
    "Acolytes",
    "Anachronauts",
    "Armadillo",
    "Armies of Titan",

    "Band of Badoon",
    "Beasty Boys",
    "Black Order",
    "Black Tom Cassidy",

    "Bomb Scare",
    "Brotherhood",
    "Brothers Grimm",
    "Children of Thanos",

    "City in Chaos",
    "Crime",
    "Crossfire's Crew",
    "Deathstrike",

    "The Doomsday Chair",
    "Down to Earth",
    "Enchantress",
    "Exodus",

    "Extreme Measures",
    "Fantasy",
    "Flight",
    "Frost Giants",

    "Future Past",
    "Goblin Gear",
    "Goblin Gimmicks",
    "Guerrilla Tactics",

    "Horror",
    "Hydra Assault",
    "Hydra Patrol",
    "The Inheritors",

    "Iron Spider's Sinister Six",
    "Kree Fanatic",
    "Kree Militants",
    "Legions of Hel",

    "Legions of Hydra",
    "Master of Time",
    "Masters of Evil",
    "Menagerie Medley",

    "A Mess of Things",
    "Military Grade",
    "Mister Hyde",
    "Mutant Insurrection",

    "Mutant Slayers",
    "Mystique",
    "Nasty Boys",
    "Osborn Tech",

    "Personal Nightmare",
    "Power Drain",
    "Ransacked Armory",
    "Reavers",

    "Running Interference",
    "Sci-Fi",
    "Sentinels",
    "Shadow King",

    "Sinister Syndicate",
    "Sitcom",
    "Space Pirates",
    "State of Emergency",

    "Streets of Mayhem",
    "Super Strength",
    "Symbiotic Strength",
    "Telepathy",

    "Temporal",
    "Under Attack",
    "Weapon Master",
    "Western",

    "Whispers of Paranoia",
    "Wrecking Crew",
    "Zero Tolerance",
    "Zzzax",

    "Badoon Headhunter",
    "Experimental Weapons",
    "Galactic Artifacts",
    "Hope Summers",

    "Infinity Gauntlet",
    "Longshot",
    "Power Stone",

    "Ship Command",
    "Sinister Assault"
  )
}