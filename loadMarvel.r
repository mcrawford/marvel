library(anytime)

LoadMarvel <- function(filename) {
  marvel           <- read.csv(filename, stringsAsFactors = FALSE)
  columnNames <- c(
    "Timestamp", "First", "FirstAspect", "IsSecondHero", "FirstTwoAspects", "IsSecondHero1", "IsSecondHero2",
    "Second", "SecondAspect", "IsThirdHero", "SecondTwoAspects", "IsThirdHero1", "IsThirdHero2",
    "Third", "ThirdAspect", "IsFourthHero", "ThirdTwoAspects", "IsFourthHero1", "IsFourthHero2",
    "Fourth", "FourthAspect", "FourthTwoAspects",
    "Scenario", "Campaign", "ExpertCampaign", "Encounter",
    "Win", "Standard", "Expert", "Heroic", "Skirmish",
    "WinWC", "DifficultyWC", "HeroicWC",
    "EpicMultiplayer", "Competitive", "OtherPods", "MainScheme1", "MainScheme2",
    "Competitive2", "MainScheme1Alt", "MainScheme2Alt", "EnemyLeader", "EnemyLeader2",
    "ResistanceModular", "RegistrationModular", "Difficulty"
  )

  # Current form exports append an unnamed FNE villain field, a spacer,
  # then Difficulty. Older exports have Difficulty immediately after column 46.
  if (ncol(marvel) >= 49) {
    extra <- marvel[, integer(0), drop = FALSE]
    if (ncol(marvel) > 49) extra <- marvel[, 50:ncol(marvel), drop = FALSE]
    if (ncol(marvel) > 49 && any(!is.na(extra) & extra != "")) {
      stop("Unexpected populated columns after Difficulty; check the source schema.")
    }
    marvel <- marvel[, 1:49]
    names(marvel) <- c(columnNames[1:46], "Villain", "Unused", "Difficulty")
    marvel$Unused <- NULL
  } else if (ncol(marvel) == 35) {
    names(marvel) <- c(columnNames[1:34], "Difficulty")
    for (name in columnNames[35:46]) marvel[[name]] <- ""
    marvel$Villain <- ""
  } else if (ncol(marvel) == 47) {
    names(marvel) <- columnNames
    marvel$Villain <- ""
  } else {
    stop("Expected a 35/47-column legacy export or a current export with at least 49 columns.")
  }
  marvel[is.na(marvel)] <- ""
  marvel$Heroic <- suppressWarnings(as.numeric(marvel$Heroic))
  marvel$HeroicWC <- suppressWarnings(as.numeric(marvel$HeroicWC))
  marvel$Skirmish <- suppressWarnings(as.numeric(marvel$Skirmish))

  # Competitive outcomes and Epic Multiplayer have different win conditions.
  excluded <- marvel$Competitive == "Yes" | marvel$Competitive2 == "Yes" |
    marvel$EpicMultiplayer == "Yes"
  if (any(excluded)) {
    warning(sprintf("Excluded %d competitive/Epic Multiplayer games from cooperative ratings.", sum(excluded)))
    marvel <- marvel[!excluded, ]
  }
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
        WinWC     = "", DifficultyWC = "", HeroicWC = 0,
        EpicMultiplayer = "", Competitive = "", OtherPods = "", MainScheme1 = "", MainScheme2 = "",
        Competitive2 = "", MainScheme1Alt = "", MainScheme2Alt = "", EnemyLeader = "", EnemyLeader2 = "",
        ResistanceModular = "", RegistrationModular = "", Difficulty = "Standard", Villain = ""
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
        WinWC     = "", DifficultyWC = "", HeroicWC = 0,
        EpicMultiplayer = "", Competitive = "", OtherPods = "", MainScheme1 = "", MainScheme2 = "",
        Competitive2 = "", MainScheme1Alt = "", MainScheme2Alt = "", EnemyLeader = "", EnemyLeader2 = "",
        ResistanceModular = "", RegistrationModular = "", Difficulty = "Expert", Villain = ""
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
        WinWC     = "", DifficultyWC = "", HeroicWC = 0,
        EpicMultiplayer = "", Competitive = "", OtherPods = "", MainScheme1 = "", MainScheme2 = "",
        Competitive2 = "", MainScheme1Alt = "", MainScheme2Alt = "", EnemyLeader = "", EnemyLeader2 = "",
        ResistanceModular = "", RegistrationModular = "", Difficulty = "Standard", Villain = ""
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
        WinWC     = "", DifficultyWC = "", HeroicWC = 0,
        EpicMultiplayer = "", Competitive = "", OtherPods = "", MainScheme1 = "", MainScheme2 = "",
        Competitive2 = "", MainScheme1Alt = "", MainScheme2Alt = "", EnemyLeader = "", EnemyLeader2 = "",
        ResistanceModular = "", RegistrationModular = "", Difficulty = "Expert", Villain = ""
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
        WinWC     = "", DifficultyWC = "", HeroicWC = 0,
        EpicMultiplayer = "", Competitive = "", OtherPods = "", MainScheme1 = "", MainScheme2 = "",
        Competitive2 = "", MainScheme1Alt = "", MainScheme2Alt = "", EnemyLeader = "", EnemyLeader2 = "",
        ResistanceModular = "", RegistrationModular = "", Difficulty = "Standard", Villain = ""
      )
      marvel <- rbind(marvel, dummyEncounterSet)
    }
  }

  marvel$DifficultyLevel <- ifelse(grepl("Expert|Extreme", marvel$Difficulty), "Expert", "Standard")
  marvel$DifficultyLevel[grepl("Standard", marvel$Standard, fixed = TRUE)]     <- "Standard"
  marvel$DifficultyLevel[grepl("Expert", marvel$Expert, fixed = TRUE)]         <- "Expert"
  marvel$DifficultyLevel[grepl("Standard", marvel$DifficultyWC, fixed = TRUE)] <- "Standard"
  marvel$DifficultyLevel[grepl("Expert", marvel$DifficultyWC, fixed = TRUE)]   <- "Expert"
  marvel$DifficultyLevel[grepl("Extreme", marvel$DifficultyWC, fixed = TRUE)]  <- "Expert"
  marvel$DifficultyLevel <- relevel(factor(marvel$DifficultyLevel), ref = "Standard")

  marvel$Standard2 <- (marvel$Standard == "Standard II (The Hood scenario pack)")
  marvel$Expert2   <- (marvel$Expert == "Expert II (The Hood scenario pack)")
  marvel$Standard3 <- (marvel$Standard == "Standard III (Age of Apocalypse campaign box)")

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
  marvel$CampaignUnus             <- (marvel$Campaign & marvel$Scenario == "Unus")
  marvel$CampaignFourHorsemen     <- (marvel$Campaign & marvel$Scenario == "Four Horsemen")
  marvel$CampaignApocalypse       <- (marvel$Campaign & marvel$Scenario == "Apocalypse")
  marvel$CampaignDarkBeast        <- (marvel$Campaign & marvel$Scenario == "Dark Beast")
  marvel$CampaignEnSabahNur       <- (marvel$Campaign & marvel$Scenario == "En Sabah Nur")
  marvel$CampaignBlackWidow       <- (marvel$Campaign & marvel$Scenario == "Black Widow")
  marvel$CampaignBatroc           <- (marvel$Campaign & marvel$Scenario == "Batroc")
  marvel$CampaignMODOK            <- (marvel$Campaign & marvel$Scenario == "M.O.D.O.K.")
  marvel$CampaignThunderbolts     <- (marvel$Campaign & marvel$Scenario == "Thunderbolts")
  marvel$CampaignBaronZemo        <- (marvel$Campaign & marvel$Scenario == "Baron Zemo")


  # Configurable Fear No Evil villains and Civil War main schemes.
  marvel$VillainBullseye <- marvel$Villain == "Bullseye"
  marvel$VillainElectro <- marvel$Villain == "Electro"
  marvel$VillainHammerhead <- marvel$Villain == "Hammerhead"
  marvel$VillainPurpleMan <- marvel$Villain == "Purple Man"
  marvel$VillainTyphoidMary <- marvel$Villain == "Typhoid Mary"
  marvel$Scheme1CutOffSupport <- marvel$MainScheme1 == "Cut Off Support" | marvel$MainScheme1Alt == "Cut Off Support"
  marvel$Scheme1EnforcetheLaw <- marvel$MainScheme1 == "Enforce the Law" | marvel$MainScheme1Alt == "Enforce the Law"
  marvel$Scheme1HomelandSecurity <- marvel$MainScheme1 == "Homeland Security" | marvel$MainScheme1Alt == "Homeland Security"
  marvel$Scheme1PublicOutrage <- marvel$MainScheme1 == "Public Outrage" | marvel$MainScheme1Alt == "Public Outrage"
  marvel$Scheme1SHIELDRecruits <- marvel$MainScheme1 == "S.H.I.E.L.D. Recruits" | marvel$MainScheme1Alt == "S.H.I.E.L.D. Recruits"
  marvel$Scheme1GatheringSupport <- marvel$MainScheme1 == "Gathering Support" | marvel$MainScheme1Alt == "Gathering Support"
  marvel$Scheme1GoingUnderground <- marvel$MainScheme1 == "Going Underground" | marvel$MainScheme1Alt == "Going Underground"
  marvel$Scheme1OpenRebellion <- marvel$MainScheme1 == "Open Rebellion" | marvel$MainScheme1Alt == "Open Rebellion"
  marvel$Scheme1ProtectSecretIdentities <- marvel$MainScheme1 == "Protect Secret Identities" | marvel$MainScheme1Alt == "Protect Secret Identities"
  marvel$Scheme1RallyingCall <- marvel$MainScheme1 == "Rallying Call" | marvel$MainScheme1Alt == "Rallying Call"
  marvel$Scheme2HuntingRebelHeroes <- marvel$MainScheme2 == "Hunting Rebel Heroes" | marvel$MainScheme2Alt == "Hunting Rebel Heroes"
  marvel$Scheme2TheInitiative <- marvel$MainScheme2 == "The Initiative" | marvel$MainScheme2Alt == "The Initiative"
  marvel$Scheme2MightyAvengers <- marvel$MainScheme2 == "Mighty Avengers" | marvel$MainScheme2Alt == "Mighty Avengers"
  marvel$Scheme2NegativeZonePrison <- marvel$MainScheme2 == "Negative Zone Prison" | marvel$MainScheme2Alt == "Negative Zone Prison"
  marvel$Scheme2NoGoingBack <- marvel$MainScheme2 == "No Going Back" | marvel$MainScheme2Alt == "No Going Back"
  marvel$Scheme2ExposeOverreach <- marvel$MainScheme2 == "Expose Overreach" | marvel$MainScheme2Alt == "Expose Overreach"
  marvel$Scheme2GuerillaWarfare <- marvel$MainScheme2 == "Guerilla Warfare" | marvel$MainScheme2Alt == "Guerilla Warfare"
  marvel$Scheme2NeighborhoodProtectors <- marvel$MainScheme2 == "Neighborhood Protectors" | marvel$MainScheme2Alt == "Neighborhood Protectors"
  marvel$Scheme2SecretAvengers <- marvel$MainScheme2 == "Secret Avengers" | marvel$MainScheme2Alt == "Secret Avengers"
  marvel$Scheme2SuperheroJailbreak <- marvel$MainScheme2 == "Superhero Jailbreak" | marvel$MainScheme2Alt == "Superhero Jailbreak"
  marvel$CampaignArtMuseumHeist <- marvel$Campaign & marvel$Scenario == "Art Museum Heist"
  marvel$CampaignTheGetaway <- marvel$Campaign & marvel$Scenario == "The Getaway"
  marvel$CampaignProtectionRacket <- marvel$Campaign & marvel$Scenario == "Protection Racket"
  marvel$CampaignTheRaftBreakout <- marvel$Campaign & marvel$Scenario == "The Raft Breakout"
  marvel$CampaignStopthePresses <- marvel$Campaign & marvel$Scenario == "Stop the Presses"
  marvel$CampaignKingpin <- marvel$Campaign & marvel$Scenario == "Kingpin"

  encounterLabels <- Map(function(a, b, c) {
    labels <- unlist(strsplit(paste(a, b, c, sep = ","), ",", fixed = TRUE))
    trimws(labels[nzchar(trimws(labels))])
  }, marvel$Encounter, marvel$ResistanceModular, marvel$RegistrationModular)
  marvel$Undeclared <- lengths(encounterLabels) == 0 & marvel$Scenario != "Wrecking Crew"
  marvel$BombScare <- vapply(encounterLabels, function(x) "Bomb Scare" %in% x, logical(1))
  marvel$MastersOfEvil <- vapply(encounterLabels, function(x) "Masters of Evil" %in% x, logical(1))
  marvel$UnderAttack <- vapply(encounterLabels, function(x) "Under Attack" %in% x, logical(1))
  marvel$LegionsOfHydra <- vapply(encounterLabels, function(x) "Legions of Hydra" %in% x, logical(1))
  marvel$DoomsdayChair <- vapply(encounterLabels, function(x) "The Doomsday Chair" %in% x, logical(1))
  marvel$GoblinGimmicks <- vapply(encounterLabels, function(x) "Goblin Gimmicks" %in% x, logical(1))
  marvel$MessOfThings <- vapply(encounterLabels, function(x) "A Mess of Things" %in% x, logical(1))
  marvel$PowerDrain <- vapply(encounterLabels, function(x) "Power Drain" %in% x, logical(1))
  marvel$RunningInterference <- vapply(encounterLabels, function(x) "Running Interference" %in% x, logical(1))
  marvel$KreeFanatic <- vapply(encounterLabels, function(x) "Kree Fanatic" %in% x, logical(1))
  marvel$HydraAssault <- vapply(encounterLabels, function(x) "Hydra Assault" %in% x, logical(1))
  marvel$HydraPatrol <- vapply(encounterLabels, function(x) "Hydra Patrol" %in% x, logical(1))
  marvel$WeaponMaster <- vapply(encounterLabels, function(x) "Weapon Master" %in% x, logical(1))
  marvel$ExperimentalWeapons <- vapply(encounterLabels, function(x) "Experimental Weapons" %in% x, logical(1))
  marvel$Temporal <- vapply(encounterLabels, function(x) "Temporal" %in% x, logical(1))
  marvel$MasterOfTime <- vapply(encounterLabels, function(x) "Master of Time" %in% x, logical(1))
  marvel$Anachronauts <- vapply(encounterLabels, function(x) "Anachronauts" %in% x, logical(1))
  marvel$BandOfBadoon <- vapply(encounterLabels, function(x) "Band of Badoon" %in% x, logical(1))
  marvel$MenagerieMedley <- vapply(encounterLabels, function(x) "Menagerie Medley" %in% x, logical(1))
  marvel$GalacticArtifacts <- vapply(encounterLabels, function(x) "Galactic Artifacts" %in% x, logical(1))
  marvel$SpacePirates <- vapply(encounterLabels, function(x) "Space Pirates" %in% x, logical(1))
  marvel$KreeMilitants <- vapply(encounterLabels, function(x) "Kree Militants" %in% x, logical(1))
  marvel$ShipCommand <- vapply(encounterLabels, function(x) "Ship Command" %in% x, logical(1))
  marvel$BadoonHeadhunter <- vapply(encounterLabels, function(x) "Badoon Headhunter" %in% x, logical(1))
  marvel$PowerStone <- vapply(encounterLabels, function(x) "Power Stone" %in% x, logical(1))
  marvel$BlackOrder <- vapply(encounterLabels, function(x) "Black Order" %in% x, logical(1))
  marvel$ArmiesOfTitan <- vapply(encounterLabels, function(x) "Armies of Titan" %in% x, logical(1))
  marvel$ChildrenOfThanos <- vapply(encounterLabels, function(x) "Children of Thanos" %in% x, logical(1))
  marvel$LegionsOfHel <- vapply(encounterLabels, function(x) "Legions of Hel" %in% x, logical(1))
  marvel$FrostGiants <- vapply(encounterLabels, function(x) "Frost Giants" %in% x, logical(1))
  marvel$Enchantress <- vapply(encounterLabels, function(x) "Enchantress" %in% x, logical(1))
  marvel$InfinityGauntlet <- vapply(encounterLabels, function(x) "Infinity Gauntlet" %in% x, logical(1))
  marvel$BeastyBoys <- vapply(encounterLabels, function(x) "Beasty Boys" %in% x, logical(1))
  marvel$BrothersGrimm <- vapply(encounterLabels, function(x) "Brothers Grimm" %in% x, logical(1))
  marvel$CrossfiresCrew <- vapply(encounterLabels, function(x) "Crossfire's Crew" %in% x, logical(1))
  marvel$MisterHyde <- vapply(encounterLabels, function(x) "Mister Hyde" %in% x, logical(1))
  marvel$RansackedArmory <- vapply(encounterLabels, function(x) "Ransacked Armory" %in% x, logical(1))
  marvel$SinisterSyndicate <- vapply(encounterLabels, function(x) "Sinister Syndicate" %in% x, logical(1))
  marvel$StateOfEmergency <- vapply(encounterLabels, function(x) "State of Emergency" %in% x, logical(1))
  marvel$StreetsOfMayhem <- vapply(encounterLabels, function(x) "Streets of Mayhem" %in% x, logical(1))
  marvel$WreckingCrew <- vapply(encounterLabels, function(x) "Wrecking Crew" %in% x, logical(1))
  marvel$CityInChaos <- vapply(encounterLabels, function(x) "City in Chaos" %in% x, logical(1))
  marvel$DownToEarth <- vapply(encounterLabels, function(x) "Down to Earth" %in% x, logical(1))
  marvel$GoblinGear <- vapply(encounterLabels, function(x) "Goblin Gear" %in% x, logical(1))
  marvel$GuerrillaTactics <- vapply(encounterLabels, function(x) "Guerrilla Tactics" %in% x, logical(1))
  marvel$OsbornTech <- vapply(encounterLabels, function(x) "Osborn Tech" %in% x, logical(1))
  marvel$PersonalNightmare <- vapply(encounterLabels, function(x) "Personal Nightmare" %in% x, logical(1))
  marvel$SinisterAssault <- vapply(encounterLabels, function(x) "Sinister Assault" %in% x, logical(1))
  marvel$SymbioticStrength <- vapply(encounterLabels, function(x) "Symbiotic Strength" %in% x, logical(1))
  marvel$WhispersOfParanoia <- vapply(encounterLabels, function(x) "Whispers of Paranoia" %in% x, logical(1))
  marvel$Armadillo <- vapply(encounterLabels, function(x) "Armadillo" %in% x, logical(1))
  marvel$Zzzax <- vapply(encounterLabels, function(x) "Zzzax" %in% x, logical(1))
  marvel$Inheritors <- vapply(encounterLabels, function(x) "The Inheritors" %in% x, logical(1))
  marvel$ISSinisterSix <- vapply(encounterLabels, function(x) "Iron Spider's Sinister Six" %in% x, logical(1))
  marvel$Deathstrike <- vapply(encounterLabels, function(x) "Deathstrike" %in% x, logical(1))
  marvel$ShadowKing <- vapply(encounterLabels, function(x) "Shadow King" %in% x, logical(1))
  marvel$Mystique <- vapply(encounterLabels, function(x) "Mystique" %in% x, logical(1))
  marvel$Brotherhood <- vapply(encounterLabels, function(x) "Brotherhood" %in% x, logical(1))
  marvel$ZeroTolerance <- vapply(encounterLabels, function(x) "Zero Tolerance" %in% x, logical(1))
  marvel$Sentinels <- vapply(encounterLabels, function(x) "Sentinels" %in% x, logical(1))
  marvel$Acolytes <- vapply(encounterLabels, function(x) "Acolytes" %in% x, logical(1))
  marvel$FuturePast <- vapply(encounterLabels, function(x) "Future Past" %in% x, logical(1))
  marvel$Crime <- vapply(encounterLabels, function(x) "Crime" %in% x, logical(1))
  marvel$Fantasy <- vapply(encounterLabels, function(x) "Fantasy" %in% x, logical(1))
  marvel$Horror <- vapply(encounterLabels, function(x) "Horror" %in% x, logical(1))
  marvel$SciFi <- vapply(encounterLabels, function(x) "Sci-Fi" %in% x, logical(1))
  marvel$Sitcom <- vapply(encounterLabels, function(x) "Sitcom" %in% x, logical(1))
  marvel$Western <- vapply(encounterLabels, function(x) "Western" %in% x, logical(1))
  marvel$Longshot <- vapply(encounterLabels, function(x) "Longshot" %in% x, logical(1))
  marvel$BlackTomCassidy <- vapply(encounterLabels, function(x) "Black Tom Cassidy" %in% x, logical(1))
  marvel$Exodus <- vapply(encounterLabels, function(x) "Exodus" %in% x, logical(1))
  marvel$ExtremeMeasures <- vapply(encounterLabels, function(x) "Extreme Measures" %in% x, logical(1))
  marvel$Flight <- vapply(encounterLabels, function(x) "Flight" %in% x, logical(1))
  marvel$MilitaryGrade <- vapply(encounterLabels, function(x) "Military Grade" %in% x, logical(1))
  marvel$MutantInsurrection <- vapply(encounterLabels, function(x) "Mutant Insurrection" %in% x, logical(1))
  marvel$MutantSlayers <- vapply(encounterLabels, function(x) "Mutant Slayers" %in% x, logical(1))
  marvel$NastyBoys <- vapply(encounterLabels, function(x) "Nasty Boys" %in% x, logical(1))
  marvel$Reavers <- vapply(encounterLabels, function(x) "Reavers" %in% x, logical(1))
  marvel$Dreadpool <- vapply(encounterLabels, function(x) "Dreadpool" %in% x, logical(1))
  marvel$SuperStrength <- vapply(encounterLabels, function(x) "Super Strength" %in% x, logical(1))
  marvel$Telepathy <- vapply(encounterLabels, function(x) "Telepathy" %in% x, logical(1))
  marvel$HopeSummers <- vapply(encounterLabels, function(x) "Hope Summers" %in% x, logical(1))
  marvel$Infinites <- vapply(encounterLabels, function(x) "Infinites" %in% x, logical(1))
  marvel$DystopianNightmare <- vapply(encounterLabels, function(x) "Dystopian Nightmare" %in% x, logical(1))
  marvel$Hounds <- vapply(encounterLabels, function(x) "Hounds" %in% x, logical(1))
  marvel$DarkRiders <- vapply(encounterLabels, function(x) "Dark Riders" %in% x, logical(1))
  marvel$SavageLand <- vapply(encounterLabels, function(x) "Savage Land" %in% x, logical(1))
  marvel$Genosha <- vapply(encounterLabels, function(x) "Genosha" %in% x, logical(1))
  marvel$BlueMoon <- vapply(encounterLabels, function(x) "Blue Moon" %in% x, logical(1))
  marvel$CelestialTech <- vapply(encounterLabels, function(x) "Celestial Tech" %in% x, logical(1))
  marvel$ClanAkkaba <- vapply(encounterLabels, function(x) "Clan Akkaba" %in% x, logical(1))
  marvel$AgeOfApocalypse <- vapply(encounterLabels, function(x) "Age of Apocalypse" %in% x, logical(1))
  marvel$Sauron <- vapply(encounterLabels, function(x) "Sauron" %in% x, logical(1))
  marvel$Arcade <- vapply(encounterLabels, function(x) "Arcade" %in% x, logical(1))
  marvel$CrazyGang <- vapply(encounterLabels, function(x) "Crazy Gang" %in% x, logical(1))
  marvel$Hellfire <- vapply(encounterLabels, function(x) "Hellfire" %in% x, logical(1))
  marvel$AIMAbduction <- vapply(encounterLabels, function(x) "A.I.M. Abduction" %in% x, logical(1))
  marvel$AIMScience <- vapply(encounterLabels, function(x) "A.I.M. Science" %in% x, logical(1))
  marvel$BatrocsBrigade <- vapply(encounterLabels, function(x) "Batroc's Brigade" %in% x, logical(1))
  marvel$ScientistSupreme <- vapply(encounterLabels, function(x) "Scientist Supreme" %in% x, logical(1))
  marvel$GravitationalPull <- vapply(encounterLabels, function(x) "Gravitational Pull" %in% x, logical(1))
  marvel$HardSound <- vapply(encounterLabels, function(x) "Hard Sound" %in% x, logical(1))
  marvel$PaleLittleSpider <- vapply(encounterLabels, function(x) "Pale Little Spider" %in% x, logical(1))
  marvel$PowerOfTheAtom <- vapply(encounterLabels, function(x) "Power of the Atom" %in% x, logical(1))
  marvel$Supersonic <- vapply(encounterLabels, function(x) "Supersonic" %in% x, logical(1))
  marvel$TheLeaper <- vapply(encounterLabels, function(x) "The Leaper" %in% x, logical(1))
  marvel$SHIELD <- vapply(encounterLabels, function(x) "S.H.I.E.L.D." %in% x, logical(1))
  marvel$TricksterMagic <- vapply(encounterLabels, function(x) "Trickster Magic" %in% x, logical(1))
  marvel$EncounterExtremeRisk <- vapply(encounterLabels, function(x) "Extreme Risk" %in% x, logical(1))
  marvel$EncounterGrowingStrong <- vapply(encounterLabels, function(x) "Growing Strong" %in% x, logical(1))
  marvel$EncounterTechno <- vapply(encounterLabels, function(x) "Techno" %in% x, logical(1))
  marvel$EncounterWhiteout <- vapply(encounterLabels, function(x) "Whiteout" %in% x, logical(1))
  marvel$EncounterAllVersusAll <- vapply(encounterLabels, function(x) "All Versus All" %in% x, logical(1))
  marvel$EncounterSHIELDExecutiveBoard <- vapply(encounterLabels, function(x) "S.H.I.E.L.D. Executive Board" %in% x, logical(1))
  marvel$EncounterCapeKiller <- vapply(encounterLabels, function(x) "Cape-Killer" %in% x, logical(1))
  marvel$EncounterDangerousRecruits <- vapply(encounterLabels, function(x) "Dangerous Recruits" %in% x, logical(1))
  marvel$EncounterDeadlyDuo <- vapply(encounterLabels, function(x) "Deadly Duo" %in% x, logical(1))
  marvel$EncounterHeroesforHire <- vapply(encounterLabels, function(x) "Heroes for Hire" %in% x, logical(1))
  marvel$EncounterTheInitiative <- vapply(encounterLabels, function(x) "The Initiative" %in% x, logical(1))
  marvel$EncounterMariaHill <- vapply(encounterLabels, function(x) "Maria Hill" %in% x, logical(1))
  marvel$EncounterMartialLaw <- vapply(encounterLabels, function(x) "Martial Law" %in% x, logical(1))
  marvel$EncounterMightyAvengers <- vapply(encounterLabels, function(x) "Mighty Avengers" %in% x, logical(1))
  marvel$EncounterPaladin <- vapply(encounterLabels, function(x) "Paladin" %in% x, logical(1))
  marvel$EncounterSHIELDOps <- vapply(encounterLabels, function(x) "S.H.I.E.L.D. Ops" %in% x, logical(1))
  marvel$EncounterTaskmaster <- vapply(encounterLabels, function(x) "Taskmaster" %in% x, logical(1))
  marvel$EncounterThunderbolts <- vapply(encounterLabels, function(x) "Thunderbolts" %in% x, logical(1))
  marvel$EncounterAtlanteans <- vapply(encounterLabels, function(x) "Atlanteans" %in% x, logical(1))
  marvel$EncounterCloakDagger <- vapply(encounterLabels, function(x) "Cloak & Dagger" %in% x, logical(1))
  marvel$EncounterDefenders <- vapply(encounterLabels, function(x) "Defenders" %in% x, logical(1))
  marvel$EncounterHellsKitchen <- vapply(encounterLabels, function(x) "Hell's Kitchen" %in% x, logical(1))
  marvel$EncounterMoonKnight <- vapply(encounterLabels, function(x) "Moon Knight" %in% x, logical(1))
  marvel$EncounterNamor <- vapply(encounterLabels, function(x) "Namor" %in% x, logical(1))
  marvel$EncounterNewAvengers <- vapply(encounterLabels, function(x) "New Avengers" %in% x, logical(1))
  marvel$EncounterRoyalGuard <- vapply(encounterLabels, function(x) "Royal Guard" %in% x, logical(1))
  marvel$EncounterScarletTwins <- vapply(encounterLabels, function(x) "Scarlet Twins" %in% x, logical(1))
  marvel$EncounterSecretAvengers <- vapply(encounterLabels, function(x) "Secret Avengers" %in% x, logical(1))
  marvel$EncounterSpiderMan <- vapply(encounterLabels, function(x) "Spider-Man" %in% x, logical(1))
  marvel$EncounterYoungAvengers <- vapply(encounterLabels, function(x) "Young Avengers" %in% x, logical(1))
  marvel$EncounterCops <- vapply(encounterLabels, function(x) "Cops" %in% x, logical(1))
  marvel$EncounterTheOwl <- vapply(encounterLabels, function(x) "The Owl" %in% x, logical(1))
  marvel$EncounterDrive <- vapply(encounterLabels, function(x) "Drive" %in% x, logical(1))
  marvel$EncounterDisasters <- vapply(encounterLabels, function(x) "Disasters" %in% x, logical(1))
  marvel$EncounterTracksuitMafia <- vapply(encounterLabels, function(x) "Tracksuit Mafia" %in% x, logical(1))
  marvel$EncounterTombstone <- vapply(encounterLabels, function(x) "Tombstone" %in% x, logical(1))

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
  marvel$Infinites[marvel$Scenario == "Unus"]                          <- TRUE

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
  marvel$Bishop <- marvel$First == "Bishop" |
    marvel$Second == "Bishop" |
    marvel$Third == "Bishop" |
    marvel$Fourth == "Bishop"
  marvel$BlackPanther <- marvel$First == "Black Panther (T'Challa)" |
    marvel$Second == "Black Panther (T'Challa)" |
    marvel$Third == "Black Panther (T'Challa)" |
    marvel$Fourth == "Black Panther (T'Challa)"
  marvel$BlackPantherShuri <- marvel$First == "Black Panther (Shuri)" |
    marvel$Second == "Black Panther (Shuri)" |
    marvel$Third == "Black Panther (Shuri)" |
    marvel$Fourth == "Black Panther (Shuri)"
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
  marvel$Falcon <- marvel$First == "Falcon" |
    marvel$Second == "Falcon" |
    marvel$Third == "Falcon" |
    marvel$Fourth == "Falcon"
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
  marvel$Iceman <- marvel$First == "Iceman" |
    marvel$Second == "Iceman" |
    marvel$Third == "Iceman" |
    marvel$Fourth == "Iceman"
  marvel$IronMan <- marvel$First == "Iron Man" |
    marvel$Second == "Iron Man" |
    marvel$Third == "Iron Man" |
    marvel$Fourth == "Iron Man"
  marvel$Ironheart <- marvel$First == "Ironheart" |
    marvel$Second == "Ironheart" |
    marvel$Third == "Ironheart" |
    marvel$Fourth == "Ironheart"
  marvel$Jubilee <- marvel$First == "Jubilee" |
    marvel$Second == "Jubilee" |
    marvel$Third == "Jubilee" |
    marvel$Fourth == "Jubilee"
  marvel$Magik <- marvel$First == "Magik" |
    marvel$Second == "Magik" |
    marvel$Third == "Magik" |
    marvel$Fourth == "Magik"
  marvel$Magneto <- marvel$First == "Magneto" |
    marvel$Second == "Magneto" |
    marvel$Third == "Magneto" |
    marvel$Fourth == "Magneto"
  marvel$MariaHill <- marvel$First == "Maria Hill" |
    marvel$Second == "Maria Hill" |
    marvel$Third == "Maria Hill" |
    marvel$Fourth == "Maria Hill"
  marvel$MsMarvel <- marvel$First == "Ms. Marvel" |
    marvel$Second == "Ms. Marvel" |
    marvel$Third == "Ms. Marvel" |
    marvel$Fourth == "Ms. Marvel"
  marvel$Nebula <- marvel$First == "Nebula" |
    marvel$Second == "Nebula" |
    marvel$Third == "Nebula" |
    marvel$Fourth == "Nebula"
  marvel$NickFury <- marvel$First == "Nick Fury" |
    marvel$Second == "Nick Fury" |
    marvel$Third == "Nick Fury" |
    marvel$Fourth == "Nick Fury"
  marvel$Nightcrawler <- marvel$First == "Nightcrawler" |
    marvel$Second == "Nightcrawler" |
    marvel$Third == "Nightcrawler" |
    marvel$Fourth == "Nightcrawler"
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
  marvel$Silk <- marvel$First == "Silk" |
    marvel$Second == "Silk" |
    marvel$Third == "Silk" |
    marvel$Fourth == "Silk"
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
  marvel$WinterSoldier <- marvel$First == "Winter Soldier" |
    marvel$Second == "Winter Soldier" |
    marvel$Third == "Winter Soldier" |
    marvel$Fourth == "Winter Soldier"
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
  marvel$BishopSolo                <- marvel$Bishop & marvel$OneHero
  marvel$BlackPantherSolo          <- marvel$BlackPanther & marvel$OneHero
  marvel$BlackPantherShuriSolo     <- marvel$BlackPantherShuri & marvel$OneHero
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
  marvel$FalconSolo                <- marvel$Falcon & marvel$OneHero
  marvel$GambitSolo                <- marvel$Gambit & marvel$OneHero
  marvel$GamoraSolo                <- marvel$Gamora & marvel$OneHero
  marvel$GhostSpiderSolo           <- marvel$GhostSpider & marvel$OneHero
  marvel$GrootSolo                 <- marvel$Groot & marvel$OneHero
  marvel$HawkeyeSolo               <- marvel$Hawkeye & marvel$OneHero
  marvel$HulkSolo                  <- marvel$Hulk & marvel$OneHero
  marvel$IcemanSolo                <- marvel$Iceman & marvel$OneHero
  marvel$IronManSolo               <- marvel$IronMan & marvel$OneHero
  marvel$IronheartSolo             <- marvel$Ironheart & marvel$OneHero
  marvel$JubileeSolo               <- marvel$Jubilee & marvel$OneHero
  marvel$MagikSolo                 <- marvel$Magik & marvel$OneHero
  marvel$MagnetoSolo               <- marvel$Magneto & marvel$OneHero
  marvel$MariaHillSolo             <- marvel$MariaHill & marvel$OneHero
  marvel$MsMarvelSolo              <- marvel$MsMarvel & marvel$OneHero
  marvel$NebulaSolo                <- marvel$Nebula & marvel$OneHero
  marvel$NickFurySolo              <- marvel$NickFury & marvel$OneHero
  marvel$NightcrawlerSolo          <- marvel$Nightcrawler & marvel$OneHero
  marvel$NovaSolo                  <- marvel$Nova & marvel$OneHero
  marvel$PhoenixSolo               <- marvel$Phoenix & marvel$OneHero
  marvel$PsylockeSolo              <- marvel$Psylocke & marvel$OneHero
  marvel$RogueSolo                 <- marvel$Rogue & marvel$OneHero
  marvel$QuicksilverSolo           <- marvel$Quicksilver & marvel$OneHero
  marvel$RocketRaccoonSolo         <- marvel$RocketRaccoon & marvel$OneHero
  marvel$ScarletWitchSolo          <- marvel$ScarletWitch & marvel$OneHero
  marvel$ShadowcatSolo             <- marvel$Shadowcat & marvel$OneHero
  marvel$SheHulkSolo               <- marvel$SheHulk & marvel$OneHero
  marvel$SilkSolo                  <- marvel$Silk & marvel$OneHero
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
  marvel$WinterSoldierSolo         <- marvel$WinterSoldier & marvel$OneHero
  marvel$WolverineSolo             <- marvel$Wolverine & marvel$OneHero
  marvel$X23Solo                   <- marvel$X23 & marvel$OneHero

  marvel$Daredevil <- marvel$First == "Daredevil" | marvel$Second == "Daredevil" |
    marvel$Third == "Daredevil" | marvel$Fourth == "Daredevil"
  marvel$DaredevilSolo <- marvel$Daredevil & marvel$OneHero
  marvel$Echo <- marvel$First == "Echo" | marvel$Second == "Echo" |
    marvel$Third == "Echo" | marvel$Fourth == "Echo"
  marvel$EchoSolo <- marvel$Echo & marvel$OneHero
  marvel$Hercules <- marvel$First == "Hercules" | marvel$Second == "Hercules" |
    marvel$Third == "Hercules" | marvel$Fourth == "Hercules"
  marvel$HerculesSolo <- marvel$Hercules & marvel$OneHero
  marvel$Hulkling <- marvel$First == "Hulkling" | marvel$Second == "Hulkling" |
    marvel$Third == "Hulkling" | marvel$Fourth == "Hulkling"
  marvel$HulklingSolo <- marvel$Hulkling & marvel$OneHero
  marvel$JessicaJones <- marvel$First == "Jessica Jones" | marvel$Second == "Jessica Jones" |
    marvel$Third == "Jessica Jones" | marvel$Fourth == "Jessica Jones"
  marvel$JessicaJonesSolo <- marvel$JessicaJones & marvel$OneHero
  marvel$LukeCage <- marvel$First == "Luke Cage" | marvel$Second == "Luke Cage" |
    marvel$Third == "Luke Cage" | marvel$Fourth == "Luke Cage"
  marvel$LukeCageSolo <- marvel$LukeCage & marvel$OneHero
  marvel$Tigra <- marvel$First == "Tigra" | marvel$Second == "Tigra" |
    marvel$Third == "Tigra" | marvel$Fourth == "Tigra"
  marvel$TigraSolo <- marvel$Tigra & marvel$OneHero
  marvel$WonderMan <- marvel$First == "Wonder Man" | marvel$Second == "Wonder Man" |
    marvel$Third == "Wonder Man" | marvel$Fourth == "Wonder Man"
  marvel$WonderManSolo <- marvel$WonderMan & marvel$OneHero

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
    "Trickster Magic",

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
    "Sinister Assault",

    "Dreadpool",

    "Infinites",
    "Dystopian Nightmare",
    "Hounds",
    "Dark Riders",
    "Savage Land",
    "Genosha",
    "Blue Moon",
    "Celestial Tech",
    "Clan Akkaba",
    "Age of Apocalypse",

    "Sauron",
    "Arcade",
    "Crazy Gang",
    "Hellfire",

    "A.I.M. Abduction",
    "A.I.M. Science",
    "Batroc's Brigade",
    "Scientist Supreme",
    "Gravitational Pull",
    "Hard Sound",
    "Pale Little Spider",
    "Power of the Atom",
    "Supersonic",
    "The Leaper",
    "S.H.I.E.L.D.",
    "Extreme Risk",
    "Growing Strong",
    "Techno",
    "Whiteout",
    "All Versus All",
    "S.H.I.E.L.D. Executive Board",
    "Cape-Killer",
    "Dangerous Recruits",
    "Deadly Duo",
    "Heroes for Hire",
    "The Initiative",
    "Maria Hill",
    "Martial Law",
    "Mighty Avengers",
    "Paladin",
    "S.H.I.E.L.D. Ops",
    "Taskmaster",
    "Thunderbolts",
    "Atlanteans",
    "Cloak & Dagger",
    "Defenders",
    "Hell's Kitchen",
    "Moon Knight",
    "Namor",
    "New Avengers",
    "Royal Guard",
    "Scarlet Twins",
    "Secret Avengers",
    "Spider-Man",
    "Young Avengers",
    "Cops",
    "The Owl",
    "Drive",
    "Disasters",
    "Tracksuit Mafia",
    "Tombstone"
  )
}
