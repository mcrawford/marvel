library(anytime)

LoadMarvel <- function(filename) {
	marvel <- read.csv(filename)
	colnames(marvel) <- c(
		"Timestamp", "First", "FirstAspect", "IsSecondHero", "FirstTwoAspects", "IsSecondHero1", "IsSecondHero2",
		"Second", "SecondAspect", "IsThirdHero", "SecondTwoAspects", "IsThirdHero1", "IsThirdHero2",
		"Third", "ThirdAspect", "IsFourthHero", "ThirdTwoAspects", "IsFourthHero1", "IsFourthHero2",
		"Fourth", "FourthAspect", "FourthTwoAspects",
		"Scenario", "Campaign", "ExpertCampaign", "Encounter", 
		"Win", "Standard", "Expert", "Heroic", "Skirmish", 
		"WinWC", "DifficultyWC", "HeroicWC", "Difficulty"
	)

	marvel$Timestamp <- anytime(marvel$Timestamp)
	marvel$Scenario <- relevel(factor(marvel$Scenario), ref="Rhino")
	marvel$Win[marvel$Win == ""] <- marvel$WinWC[marvel$Win == ""]
	marvel$Heroic[is.na(marvel$Heroic)] <- marvel$HeroicWC[is.na(marvel$Heroic)]

    meanDate <- mean(marvel$Timestamp)
	for (i in 1:20) {
		for (s in levels(marvel$Scenario)) {
			multiStandard <- data.frame(
				Timestamp = meanDate,
				First = "", FirstAspect = "", IsSecondHero = "Yes", FirstTwoAspects ="", IsSecondHero1 = "No", IsSecondHero2 = "No",
				Second = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
				Third = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
				Fourth = "", FourthAspect = "", FourthTwoAspects = "",
				Scenario = s,
				Campaign = "No", ExpertCampaign = "No", Encounter = "",
				Win = if (i <= 16) "Yes" else "No", Standard = "Standard (Core Set)", Expert = "", Heroic = 0, Skirmish = 0,
				WinWC = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Standard"
			)
			multiExpert <- data.frame(
				Timestamp = meanDate,
				First = "", FirstAspect = "", IsSecondHero = "Yes", FirstTwoAspects ="", IsSecondHero1 = "No", IsSecondHero2 = "No",
				Second = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
				Third = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
				Fourth = "", FourthAspect = "", FourthTwoAspects = "",
				Scenario = s,
				Campaign = "No", ExpertCampaign = "No", Encounter = "",
				Win = if (i <= 9) "Yes" else "No", Standard = "Standard (Core Set)", Expert = "Expert (Core Set)", Heroic = 0, Skirmish = 0,
				WinWC = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Expert"
			)
			soloStandard <- data.frame(
				Timestamp = meanDate,
				First = "", FirstAspect = "", IsSecondHero = "No", FirstTwoAspects ="", IsSecondHero1 = "No", IsSecondHero2 = "No",
				Second = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
				Third = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
				Fourth = "", FourthAspect = "", FourthTwoAspects = "",
				Scenario = s,
				Campaign = "No", ExpertCampaign = "No", Encounter = "",
				Win = if (i <= 15) "Yes" else "No", Standard = "Standard (Core Set)", Expert = "", Heroic = 0, Skirmish = 0,
				WinWC = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Standard"
			)
			soloExpert <- data.frame(
				Timestamp = meanDate,
				First = "", FirstAspect = "", IsSecondHero = "No", FirstTwoAspects ="", IsSecondHero1 = "No", IsSecondHero2 = "No",
				Second = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
				Third = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
				Fourth = "", FourthAspect = "", FourthTwoAspects = "",
				Scenario = s,
				Campaign = "No", ExpertCampaign = "No", Encounter = "",
				Win = if (i <= 8) "Yes" else "No", Standard = "Standard (Core Set)", Expert = "Expert (Core Set)", Heroic = 0, Skirmish = 0,
				WinWC = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Expert"
			)
			marvel <- rbind(marvel, multiStandard)
			marvel <- rbind(marvel, multiExpert)
			marvel <- rbind(marvel, soloStandard)
			marvel <- rbind(marvel, soloExpert)
		}
	}

	marvel$DifficultyLevel <- "Standard"
	marvel$DifficultyLevel[grepl("Standard", marvel$Standard, fixed=TRUE)] <- "Standard"
	marvel$DifficultyLevel[grepl("Expert", marvel$Expert, fixed=TRUE)] <- "Expert"
	marvel$DifficultyLevel[grepl("Standard", marvel$DifficultyWC, fixed=TRUE)] <- "Standard"
	marvel$DifficultyLevel[grepl("Expert", marvel$DifficultyWC, fixed=TRUE)] <- "Expert"
	marvel$DifficultyLevel[grepl("Extreme", marvel$DifficultyWC, fixed=TRUE)] <- "Expert"
	marvel$DifficultyLevel <- relevel(factor(marvel$DifficultyLevel), ref="Standard")

	marvel$Standard2 <- (marvel$Standard == "Standard II (The Hood scenario pack)")
	marvel$Expert2 <- (marvel$Expert == "Expert II (The Hood scenario pack)")

	marvel$OneHero <- (marvel$IsSecondHero == "No")
	marvel$OneHero[marvel$First == "Spider-Woman"] <- (marvel$IsSecondHero1[marvel$First == "Spider-Woman"] == "No")
	marvel$OneHero[marvel$First == "Adam Warlock"] <- (marvel$IsSecondHero2[marvel$First == "Adam Warlock"] == "No")

	marvel$Heroic[is.na(marvel$Heroic)] <- 0
	marvel$Heroic[marvel$Heroic > 3] <- 3
	marvel$Skirmish[is.na(marvel$Skirmish)] <- 0
	marvel$SkirmishLevel = marvel$Skirmish
	marvel$Skirmish = (marvel$Skirmish > 0)
	marvel$Campaign <- (marvel$Campaign == "Yes")
	marvel$CampaignAbsorbingMan <- (marvel$Campaign & marvel$Scenario == "Absorbing Man")
	marvel$CampaignTaskmaster <- (marvel$Campaign & marvel$Scenario == "Taskmaster")
	marvel$CampaignZola <- (marvel$Campaign & marvel$Scenario == "Zola")
	marvel$CampaignRedSkull <- (marvel$Campaign & marvel$Scenario == "Red Skull")
	marvel$CampaignBrotherhood <- (marvel$Campaign & marvel$Scenario == "Brotherhood of Badoon")
	marvel$CampaignInfiltrateMuseum <- (marvel$Campaign & marvel$Scenario == "Infiltrate the Museum")
	marvel$CampaignEscapeMuseum <- (marvel$Campaign & marvel$Scenario == "Escape the Museum")
	marvel$CampaignNebula <- (marvel$Campaign & marvel$Scenario == "Nebula")
	marvel$CampaignRonan <- (marvel$Campaign & marvel$Scenario == "Ronan the Accuser")
	marvel$CampaignEbonyMaw <- (marvel$Campaign & marvel$Scenario == "Ebony Maw")
	marvel$CampaignTowerDefense <- (marvel$Campaign & marvel$Scenario == "Tower Defense")
	marvel$CampaignThanos <- (marvel$Campaign & marvel$Scenario == "Thanos")
	marvel$CampaignHela <- (marvel$Campaign & marvel$Scenario == "Hela")
	marvel$CampaignLoki <- (marvel$Campaign & marvel$Scenario == "Loki")
	marvel$CampaignSandman <- (marvel$Campaign & marvel$Scenario == "Sandman")
	marvel$CampaignVenom <- (marvel$Campaign & marvel$Scenario == "Venom")
	marvel$CampaignMysterio <- (marvel$Campaign & marvel$Scenario == "Mysterio")
	marvel$CampaignSinisterSix <- (marvel$Campaign & marvel$Scenario == "The Sinister Six")
	marvel$CampaignVenomGoblin <- (marvel$Campaign & marvel$Scenario == "VenomGoblin")

	marvel$Undeclared <- marvel$Encounter == "" & marvel$Scenario != "Wrecking Crew"
	marvel$BombScare <- grepl("Bomb Scare", marvel$Encounter, fixed=TRUE)
	marvel$MastersOfEvil <- grepl("Masters of Evil", marvel$Encounter, fixed=TRUE)
	marvel$UnderAttack <- grepl("Under Attack", marvel$Encounter, fixed=TRUE)
	marvel$LegionsOfHydra <- grepl("Legions of Hydra", marvel$Encounter, fixed=TRUE)
	marvel$DoomsdayChair <- grepl("The Doomsday Chair", marvel$Encounter, fixed=TRUE)
	marvel$GoblinGimmicks <- grepl("Goblin Gimmicks", marvel$Encounter, fixed=TRUE)
	marvel$MessOfThings <- grepl("A Mess of Things", marvel$Encounter, fixed=TRUE)
	marvel$PowerDrain <- grepl("Power Drain", marvel$Encounter, fixed=TRUE)
	marvel$RunningInterference <- grepl("Running Interference", marvel$Encounter, fixed=TRUE)
	marvel$KreeFanatic <- grepl("Kree Fanatic", marvel$Encounter, fixed=TRUE)
	marvel$HydraAssault <- grepl("Hydra Assault", marvel$Encounter, fixed=TRUE)
	marvel$HydraPatrol <- grepl("Hydra Patrol", marvel$Encounter, fixed=TRUE)
	marvel$WeaponMaster <- grepl("Weapon Master", marvel$Encounter, fixed=TRUE)
	marvel$ExperimentalWeapons <- grepl("Experimental Weapons", marvel$Encounter, fixed=TRUE)
	marvel$Temporal <- grepl("Temporal", marvel$Encounter, fixed=TRUE)
	marvel$MasterOfTime <- grepl("Master of Time", marvel$Encounter, fixed=TRUE)
	marvel$Anachronauts <- grepl("Anachronauts", marvel$Encounter, fixed=TRUE)
	marvel$BandOfBadoon <- grepl("Band of Badoon", marvel$Encounter, fixed=TRUE)
	marvel$MenagerieMedley <- grepl("Menagerie Medley", marvel$Encounter, fixed=TRUE)
	marvel$GalacticArtifacts <- grepl("Galactic Artifacts", marvel$Encounter, fixed=TRUE)
	marvel$SpacePirates <- grepl("Space Pirates", marvel$Encounter, fixed=TRUE)
	marvel$KreeMilitants <- grepl("Kree Militants", marvel$Encounter, fixed=TRUE)
	marvel$ShipCommand <- grepl("Ship Command", marvel$Encounter, fixed=TRUE)
	marvel$BadoonHeadhunter <- grepl("Badoon Headhunter", marvel$Encounter, fixed=TRUE)
	marvel$BlackOrder <- grepl("Black Order", marvel$Encounter, fixed=TRUE)
	marvel$ArmiesOfTitan <- grepl("Armies of Titan", marvel$Encounter, fixed=TRUE)
	marvel$ChildrenOfThanos <- grepl("Children of Thanos", marvel$Encounter, fixed=TRUE)
	marvel$LegionsOfHel <- grepl("Legions of Hel", marvel$Encounter, fixed=TRUE)
	marvel$FrostGiants <- grepl("Frost Giants", marvel$Encounter, fixed=TRUE)
    marvel$Enchantress <- grepl("Enchantress", marvel$Encounter, fixed=TRUE)
    marvel$InfinityGauntlet <- grepl("Infinity Gauntlet", marvel$Encounter, fixed=TRUE)
    marvel$BeastyBoys <- grepl("Beasty Boys", marvel$Encounter, fixed=TRUE)
    marvel$BrothersGrimm <- grepl("Brothers Grimm", marvel$Encounter, fixed=TRUE)
    marvel$CrossfiresCrew <- grepl("Crossfire's Crew", marvel$Encounter, fixed=TRUE)
    marvel$MisterHyde <- grepl("Mister Hyde", marvel$Encounter, fixed=TRUE)
    marvel$RansackedArmory <- grepl("Ransacked Armory", marvel$Encounter, fixed=TRUE)
    marvel$SinisterSyndicate <- grepl("Sinister Syndicate", marvel$Encounter, fixed=TRUE)
    marvel$StateOfEmergency <- grepl("State of Emergency", marvel$Encounter, fixed=TRUE)
    marvel$StreetsOfMayhem <- grepl("Streets of Mayhem", marvel$Encounter, fixed=TRUE)
    marvel$WreckingCrew <- grepl("Wrecking Crew", marvel$Encounter, fixed=TRUE)
    marvel$CityInChaos <- grepl("City in Chaos", marvel$Encounter, fixed=TRUE)
    marvel$DownToEarth <- grepl("Down to Earth", marvel$Encounter, fixed=TRUE)
    marvel$GoblinGear <- grepl("Goblin Gear", marvel$Encounter, fixed=TRUE)
    marvel$GuerrillaTactics <- grepl("Guerrilla Tactics", marvel$Encounter, fixed=TRUE)
    marvel$OsbornTech <- grepl("Osborn Tech", marvel$Encounter, fixed=TRUE)
    marvel$PersonalNightmare <- grepl("Personal Nightmare", marvel$Encounter, fixed=TRUE)
    marvel$SinisterAssault <- grepl("Sinister Assault", marvel$Encounter, fixed=TRUE)
    marvel$SymbioticStrength <- grepl("Symbiotic Strength", marvel$Encounter, fixed=TRUE)
    marvel$WhispersOfParanoia <- grepl("Whispers of Paranoia", marvel$Encounter, fixed=TRUE)
    marvel$Armadillo <- grepl("Armadillo", marvel$Encounter, fixed=TRUE)
    marvel$Zzzax <- grepl("Zzzax", marvel$Encounter, fixed=TRUE)

    marvel$HydraPatrol[marvel$Scenario == "Taskmaster"] <- TRUE
    marvel$GalacticArtifacts[marvel$Scenario == "Infiltrate the Museum"] <- TRUE
    marvel$GalacticArtifacts[marvel$Scenario == "Escape the Museum"] <- TRUE
    marvel$ShipCommand[marvel$Scenario == "Brotherhood of Badoon"] <- TRUE
    marvel$ShipCommand[marvel$Scenario == "Nebula"] <- TRUE
    marvel$ShipCommand[marvel$Scenario == "Ronan the Accuser"] <- TRUE
    marvel$InfinityGauntlet[marvel$Scenario == "Thanos"] <- TRUE
    marvel$InfinityGauntlet[marvel$Scenario == "Loki"] <- TRUE
    marvel$CityInChaos[marvel$Scenario == "Sandman"] <- TRUE
    marvel$SymbioticStrength[marvel$Scenario == "Venom"] <- TRUE
    marvel$PersonalNightmare[marvel$Scenario == "Mysterio"] <- TRUE
    marvel$SymbioticStrength[marvel$Scenario == "Venom Goblin"] <- TRUE

	marvel$Aggression <- marvel$FirstAspect == "Aggression" | marvel$SecondAspect == "Aggression" | 
						 marvel$ThirdAspect == "Aggression" | marvel$FourthAspect == "Aggression"
	marvel$Justice    <- marvel$FirstAspect == "Justice" | marvel$SecondAspect == "Justice" | 
						 marvel$ThirdAspect == "Justice" | marvel$FourthAspect == "Justice"
	marvel$Leadership <- marvel$FirstAspect == "Leadership" | marvel$SecondAspect == "Leadership" | 
						 marvel$ThirdAspect == "Leadership" | marvel$FourthAspect == "Leadership"
	marvel$Protection <- marvel$FirstAspect == "Protection" | marvel$SecondAspect == "Protection" | 
						 marvel$ThirdAspect == "Protection" | marvel$FourthAspect == "Protection"

	marvel$AdamWarlock <- marvel$First == "Adam Warlock" | marvel$Second == "Adam Warlock" | 
						 marvel$Third == "Adam Warlock" | marvel$Fourth == "Adam Warlock"
	marvel$AntMan <- marvel$First == "Ant-Man" | marvel$Second == "Ant-Man" | 
						 marvel$Third == "Ant-Man" | marvel$Fourth == "Ant-Man"
	marvel$BlackPanther <- marvel$First == "Black Panther" | marvel$Second == "Black Panther" | 
						 marvel$Third == "Black Panther" | marvel$Fourth == "Black Panther"
	marvel$BlackWidow <- marvel$First == "Black Widow" | marvel$Second == "Black Widow" | 
						 marvel$Third == "Black Widow" | marvel$Fourth == "Black Widow"
	marvel$CaptainAmerica <- marvel$First == "Captain America" | marvel$Second == "Captain America" | 
						 marvel$Third == "Captain America" | marvel$Fourth == "Captain America"
	marvel$CaptainMarvel <- marvel$First == "Captain Marvel" | marvel$Second == "Captain Marvel" | 
						 marvel$Third == "Captain Marvel" | marvel$Fourth == "Captain Marvel"
	marvel$DoctorStrange <- marvel$First == "Doctor Strange" | marvel$Second == "Doctor Strange" | 
						 marvel$Third == "Doctor Strange" | marvel$Fourth == "Doctor Strange"
	marvel$Drax <- marvel$First == "Drax" | marvel$Second == "Drax" | 
						 marvel$Third == "Drax" | marvel$Fourth == "Drax"
	marvel$Gamora <- marvel$First == "Gamora" | marvel$Second == "Gamora" | 
						 marvel$Third == "Gamora" | marvel$Fourth == "Gamora"
	marvel$GhostSpider <- marvel$First == "Ghost-Spider" | marvel$Second == "Ghost-Spider" |
						 marvel$Third == "Ghost-Spider" | marvel$Fourth == "Ghost-Spider"
	marvel$Groot <- marvel$First == "Groot" | marvel$Second == "Groot" |
						 marvel$Third == "Groot" | marvel$Fourth == "Groot"
	marvel$Hawkeye <- marvel$First == "Hawkeye" | marvel$Second == "Hawkeye" | 
						 marvel$Third == "Hawkeye" | marvel$Fourth == "Hawkeye"
	marvel$Hulk <- marvel$First == "Hulk" | marvel$Second == "Hulk" | 
						 marvel$Third == "Hulk" | marvel$Fourth == "Hulk"
	marvel$IronMan <- marvel$First == "Iron Man" | marvel$Second == "Iron Man" | 
						 marvel$Third == "Iron Man" | marvel$Fourth == "Iron Man"
	marvel$Ironheart <- marvel$First == "Ironheart" | marvel$Second == "Ironheart" |
						 marvel$Third == "Ironheart" | marvel$Fourth == "Ironheart"
	marvel$MsMarvel <- marvel$First == "Ms. Marvel" | marvel$Second == "Ms. Marvel" |
						 marvel$Third == "Ms. Marvel" | marvel$Fourth == "Ms. Marvel"
	marvel$Nebula <- marvel$First == "Nebula" | marvel$Second == "Nebula" |
						 marvel$Third == "Nebula" | marvel$Fourth == "Nebula"
	marvel$Nova <- marvel$First == "Nova" | marvel$Second == "Nova" |
						 marvel$Third == "Nova" | marvel$Fourth == "Nova"
	marvel$Quicksilver <- marvel$First == "Quicksilver" | marvel$Second == "Quicksilver" |
						 marvel$Third == "Quicksilver" | marvel$Fourth == "Quicksilver"
	marvel$RocketRaccoon <- marvel$First == "Rocket Raccoon" | marvel$Second == "Rocket Raccoon" | 
						 marvel$Third == "Rocket Raccoon" | marvel$Fourth == "Rocket Raccoon"
	marvel$ScarletWitch <- marvel$First == "Scarlet Witch" | marvel$Second == "Scarlet Witch" | 
						 marvel$Third == "Scarlet Witch" | marvel$Fourth == "Scarlet Witch"
	marvel$SheHulk <- marvel$First == "She-Hulk" | marvel$Second == "She-Hulk" | 
						 marvel$Third == "She-Hulk" | marvel$Fourth == "She-Hulk"
	marvel$Spectrum <- marvel$First == "Spectrum" | marvel$Second == "Spectrum" | 
						 marvel$Third == "Spectrum" | marvel$Fourth == "Spectrum"
	marvel$SpiderMan <- marvel$First == "Spider-Man (Peter Parker)" | marvel$Second == "Spider-Man (Peter Parker)" | 
						 marvel$Third == "Spider-Man (Peter Parker)" | marvel$Fourth == "Spider-Man (Peter Parker)"
	marvel$SpiderManMilesMorales <- marvel$First == "Spider-Man (Miles Morales)" | marvel$Second == "Spider-Man (Miles Morales)" |
						 marvel$Third == "Spider-Man (Miles Morales)" | marvel$Fourth == "Spider-Man (Miles Morales)"
	marvel$SpiderWoman <- marvel$First == "Spider-Woman" | marvel$Second == "Spider-Woman" |
						 marvel$Third == "Spider-Woman" | marvel$Fourth == "Spider-Woman"
	marvel$StarLord <- marvel$First == "Star-Lord" | marvel$Second == "Star-Lord" | 
						 marvel$Third == "Star-Lord" | marvel$Fourth == "Star-Lord"
	marvel$Thor <- marvel$First == "Thor" | marvel$Second == "Thor" | 
						 marvel$Third == "Thor" | marvel$Fourth == "Thor"
	marvel$Valkyrie <- marvel$First == "Valkyrie" | marvel$Second == "Valkyrie" |
						 marvel$Third == "Valkyrie" | marvel$Fourth == "Valkyrie"
	marvel$Venom <- marvel$First == "Venom" | marvel$Second == "Venom" |
						 marvel$Third == "Venom" | marvel$Fourth == "Venom"
	marvel$Vision <- marvel$First == "Vision" | marvel$Second == "Vision" |
						 marvel$Third == "Vision" | marvel$Fourth == "Vision"
	marvel$WarMachine <- marvel$First == "War Machine" | marvel$Second == "War Machine" |
						 marvel$Third == "War Machine" | marvel$Fourth == "War Machine"
	marvel$Wasp <- marvel$First == "Wasp" | marvel$Second == "Wasp" |
						 marvel$Third == "Wasp" | marvel$Fourth == "Wasp"

	marvel$AdamWarlockSolo <- marvel$AdamWarlock & marvel$OneHero
	marvel$AntManSolo <- marvel$AntMan & marvel$OneHero
	marvel$BlackPantherSolo <- marvel$BlackPanther & marvel$OneHero
	marvel$BlackWidowSolo <- marvel$BlackWidow & marvel$OneHero
	marvel$CaptainAmericaSolo <- marvel$CaptainAmerica & marvel$OneHero
	marvel$CaptainMarvelSolo <- marvel$CaptainMarvel & marvel$OneHero
	marvel$DoctorStrangeSolo <- marvel$DoctorStrange & marvel$OneHero
	marvel$DraxSolo <- marvel$Drax & marvel$OneHero
	marvel$GamoraSolo <- marvel$Gamora & marvel$OneHero
	marvel$GhostSpiderSolo <- marvel$GhostSpider & marvel$OneHero
	marvel$GrootSolo <- marvel$Groot & marvel$OneHero
	marvel$HawkeyeSolo <- marvel$Hawkeye & marvel$OneHero
	marvel$HulkSolo <- marvel$Hulk & marvel$OneHero
	marvel$IronManSolo <- marvel$IronMan & marvel$OneHero
	marvel$IronheartSolo <- marvel$Ironheart & marvel$OneHero
	marvel$MsMarvelSolo <- marvel$MsMarvel & marvel$OneHero
	marvel$NebulaSolo <- marvel$Nebula & marvel$OneHero
	marvel$NovaSolo <- marvel$Nova & marvel$OneHero
	marvel$QuicksilverSolo <- marvel$Quicksilver & marvel$OneHero
	marvel$RocketRaccoonSolo <- marvel$RocketRaccoon & marvel$OneHero
	marvel$ScarletWitchSolo <- marvel$ScarletWitch & marvel$OneHero
	marvel$SheHulkSolo <- marvel$SheHulk & marvel$OneHero
	marvel$SpectrumSolo <- marvel$Spectrum & marvel$OneHero
	marvel$SpiderManSolo <- marvel$SpiderMan & marvel$OneHero
	marvel$SpiderManMilesMoralesSolo <- marvel$SpiderManMilesMorales & marvel$OneHero
	marvel$SpiderWomanSolo <- marvel$SpiderWoman & marvel$OneHero
	marvel$StarLordSolo <- marvel$StarLord & marvel$OneHero
	marvel$ThorSolo <- marvel$Thor & marvel$OneHero
	marvel$ValkyrieSolo <- marvel$Valkyrie & marvel$OneHero
	marvel$VisionSolo <- marvel$Vision & marvel$OneHero
	marvel$WarMachineSolo <- marvel$WarMachine & marvel$OneHero
	marvel$WaspSolo <- marvel$Wasp & marvel$OneHero
	marvel$VenomSolo <- marvel$Venom & marvel$OneHero

	return(marvel)
}

MarvelGlm <- function(marvel) {
	return(glm(formula = (Win == "No") ~ 
		Scenario * DifficultyLevel * OneHero +
		Undeclared + Timestamp +
    	BombScare + MastersOfEvil + UnderAttack + LegionsOfHydra + DoomsdayChair +
    	GoblinGimmicks + MessOfThings + PowerDrain + RunningInterference +
    	KreeFanatic +
		ExperimentalWeapons + HydraAssault + HydraPatrol + WeaponMaster +
		Temporal + MasterOfTime + Anachronauts +
		BandOfBadoon + MenagerieMedley + GalacticArtifacts + SpacePirates + KreeMilitants +
		ShipCommand + BadoonHeadhunter +
		BlackOrder + ArmiesOfTitan + ChildrenOfThanos + LegionsOfHel + FrostGiants + Enchantress + InfinityGauntlet +
		BeastyBoys + BrothersGrimm + CrossfiresCrew + MisterHyde + RansackedArmory +
		SinisterSyndicate + StateOfEmergency + StreetsOfMayhem + WreckingCrew +
		CityInChaos + DownToEarth + SymbioticStrength + PersonalNightmare + WhispersOfParanoia +
		GuerrillaTactics + SinisterAssault + GoblinGear + OsbornTech + 
    	Heroic + factor(SkirmishLevel) + Standard2 + Expert2 +
		CampaignAbsorbingMan + CampaignTaskmaster + CampaignZola + CampaignRedSkull +
		CampaignBrotherhood + CampaignInfiltrateMuseum + CampaignEscapeMuseum + CampaignNebula + CampaignRonan +
		CampaignEbonyMaw + CampaignTowerDefense + CampaignThanos + CampaignHela + CampaignLoki +
		CampaignSandman + CampaignVenom + CampaignMysterio + CampaignSinisterSix + CampaignVenomGoblin + 
		Aggression + Justice + Leadership + Protection +
		AdamWarlock + AntMan + BlackPanther + 
		BlackWidow + CaptainAmerica + CaptainMarvel +
		DoctorStrange + Drax + Gamora + GhostSpider + Groot +
		Hawkeye + Hulk + IronMan + Ironheart + Nebula + Nova +
		MsMarvel + Quicksilver +
		RocketRaccoon + ScarletWitch + SheHulk + Spectrum + 
		SpiderMan + SpiderManMilesMorales + SpiderWoman + StarLord +
		Thor + Valkyrie + Vision + WarMachine + Wasp + Venom +
		AdamWarlockSolo + AntManSolo + BlackPantherSolo +
		BlackWidowSolo + CaptainAmericaSolo + CaptainMarvelSolo +
		DoctorStrangeSolo + DraxSolo + GamoraSolo + GhostSpiderSolo + GrootSolo +
		HawkeyeSolo + HulkSolo + IronManSolo + IronheartSolo + NebulaSolo + NovaSolo +
		MsMarvelSolo + QuicksilverSolo +
		RocketRaccoonSolo + ScarletWitchSolo + SheHulkSolo + SpectrumSolo +
		SpiderManSolo + SpiderManMilesMoralesSolo + SpiderWomanSolo + StarLordSolo +
		ThorSolo + ValkyrieSolo + VisionSolo + WarMachineSolo + WaspSolo + VenomSolo
    	, family = binomial, data = marvel))
}

MarvelFactors <- function(extendedGlm) {
	coefs <- coef(extendedGlm)
	standard = c(
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
			coefs["ScenarioVenom Goblin"]
		)
	expert = c(
			0 + coefs["DifficultyLevelExpert"], # Rhino
			coefs["ScenarioKlaw"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioKlaw:DifficultyLevelExpert"],
			coefs["ScenarioUltron"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioUltron:DifficultyLevelExpert"],
			coefs["ScenarioWrecking Crew"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioWrecking Crew:DifficultyLevelExpert"],
			coefs["ScenarioRisky Business"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioRisky Business:DifficultyLevelExpert"],
			coefs["ScenarioMutagen Formula"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioMutagen Formula:DifficultyLevelExpert"],
			coefs["ScenarioCrossbones"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioCrossbones:DifficultyLevelExpert"],
			coefs["ScenarioAbsorbing Man"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioAbsorbing Man:DifficultyLevelExpert"],
			coefs["ScenarioTaskmaster"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioTaskmaster:DifficultyLevelExpert"],
			coefs["ScenarioZola"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioZola:DifficultyLevelExpert"],
			coefs["ScenarioRed Skull"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioRed Skull:DifficultyLevelExpert"],
			coefs["ScenarioKang"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioKang:DifficultyLevelExpert"],
			coefs["ScenarioBrotherhood of Badoon"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioBrotherhood of Badoon:DifficultyLevelExpert"],
			coefs["ScenarioInfiltrate the Museum"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioInfiltrate the Museum:DifficultyLevelExpert"],
			coefs["ScenarioEscape the Museum"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioEscape the Museum:DifficultyLevelExpert"],
			coefs["ScenarioNebula"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioNebula:DifficultyLevelExpert"],
			coefs["ScenarioRonan the Accuser"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioRonan the Accuser:DifficultyLevelExpert"],
			coefs["ScenarioEbony Maw"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioEbony Maw:DifficultyLevelExpert"],
			coefs["ScenarioTower Defense"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioTower Defense:DifficultyLevelExpert"],
			coefs["ScenarioThanos"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioThanos:DifficultyLevelExpert"],
			coefs["ScenarioHela"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioHela:DifficultyLevelExpert"],
			coefs["ScenarioLoki"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioLoki:DifficultyLevelExpert"],
			coefs["ScenarioThe Hood"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioThe Hood:DifficultyLevelExpert"],
			coefs["ScenarioSandman"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioSandman:DifficultyLevelExpert"],
			coefs["ScenarioVenom"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioVenom:DifficultyLevelExpert"],
			coefs["ScenarioMysterio"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioMysterio:DifficultyLevelExpert"],
			coefs["ScenarioThe Sinister Six"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioThe Sinister Six:DifficultyLevelExpert"],
			coefs["ScenarioVenom Goblin"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioVenom Goblin:DifficultyLevelExpert"]
		)
	standardOneHero = c(
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
			coefs["ScenarioVenom Goblin"] + coefs["OneHeroTRUE"] + coefs["ScenarioVenom Goblin:OneHeroTRUE"]
		)
	expertOneHero = c(
			0 + coefs["DifficultyLevelExpert"] + coefs["OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"], # Rhino
			coefs["ScenarioKlaw"] + coefs["OneHeroTRUE"] + coefs["ScenarioKlaw:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioKlaw:DifficultyLevelExpert"] + coefs["ScenarioKlaw:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioUltron"] + coefs["OneHeroTRUE"] + coefs["ScenarioUltron:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] +
				coefs["ScenarioUltron:DifficultyLevelExpert"] + coefs["ScenarioUltron:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioWrecking Crew"] + coefs["OneHeroTRUE"] + coefs["ScenarioWrecking Crew:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioWrecking Crew:DifficultyLevelExpert"] + coefs["ScenarioWrecking Crew:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioRisky Business"] + coefs["OneHeroTRUE"] + 
				coefs["ScenarioRisky Business:OneHeroTRUE"] + 
				coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioRisky Business:DifficultyLevelExpert"] + 
				coefs["ScenarioRisky Business:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioMutagen Formula"] + coefs["OneHeroTRUE"] + 
				coefs["ScenarioMutagen Formula:OneHeroTRUE"] + 
				coefs["DifficultyLevelExpert"] +
				coefs["ScenarioMutagen Formula:DifficultyLevelExpert"] + 
				coefs["ScenarioMutagen Formula:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioCrossbones"] + coefs["OneHeroTRUE"] + coefs["ScenarioCrossbones:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioCrossbones:DifficultyLevelExpert"] + coefs["ScenarioCrossbones:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioAbsorbing Man"] + coefs["OneHeroTRUE"] + coefs["ScenarioAbsorbing Man:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioAbsorbing Man:DifficultyLevelExpert"] + coefs["ScenarioAbsorbing Man:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioTaskmaster"] + coefs["OneHeroTRUE"] + coefs["ScenarioTaskmaster:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioTaskmaster:DifficultyLevelExpert"] + coefs["ScenarioTaskmaster:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioZola"] + coefs["OneHeroTRUE"] + coefs["ScenarioZola:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioZola:DifficultyLevelExpert"] + coefs["ScenarioZola:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioRed Skull"] + coefs["OneHeroTRUE"] + coefs["ScenarioRed Skull:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioRed Skull:DifficultyLevelExpert"] + coefs["ScenarioRed Skull:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioKang"] + coefs["OneHeroTRUE"] + coefs["ScenarioKang:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioKang:DifficultyLevelExpert"] + coefs["ScenarioKang:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioBrotherhood of Badoon"] + coefs["OneHeroTRUE"] + coefs["ScenarioBrotherhood of Badoon:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioBrotherhood of Badoon:DifficultyLevelExpert"] + coefs["ScenarioBrotherhood of Badoon:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioInfiltrate the Museum"] + coefs["OneHeroTRUE"] + coefs["ScenarioInfiltrate the Museum:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioInfiltrate the Museum:DifficultyLevelExpert"] + coefs["ScenarioInfiltrate the Museum:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioEscape the Museum"] + coefs["OneHeroTRUE"] + coefs["ScenarioEscape the Museum:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioEscape the Museum:DifficultyLevelExpert"] + coefs["ScenarioEscape the Museum:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioNebula"] + coefs["OneHeroTRUE"] + coefs["ScenarioNebula:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioNebula:DifficultyLevelExpert"] + coefs["ScenarioNebula:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioRonan the Accuser"] + coefs["OneHeroTRUE"] + coefs["ScenarioRonan the Accuser:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioRonan the Accuser:DifficultyLevelExpert"] + coefs["ScenarioRonan the Accuser:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioEbony Maw"] + coefs["OneHeroTRUE"] + coefs["ScenarioEbony Maw:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioEbony Maw:DifficultyLevelExpert"] + coefs["ScenarioEbony Maw:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioTower Defense"] + coefs["OneHeroTRUE"] + coefs["ScenarioTower Defense:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioTower Defense:DifficultyLevelExpert"] + coefs["ScenarioTower Defense:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioThanos"] + coefs["OneHeroTRUE"] + coefs["ScenarioThanos:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioThanos:DifficultyLevelExpert"] + coefs["ScenarioThanos:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioHela"] + coefs["OneHeroTRUE"] + coefs["ScenarioHela:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioHela:DifficultyLevelExpert"] + coefs["ScenarioHela:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioLoki"] + coefs["OneHeroTRUE"] + coefs["ScenarioLoki:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioLoki:DifficultyLevelExpert"] + coefs["ScenarioLoki:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioThe Hood"] + coefs["OneHeroTRUE"] + coefs["ScenarioThe Hood:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] +
				coefs["ScenarioThe Hood:DifficultyLevelExpert"] + coefs["ScenarioThe Hood:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioSandman"] + coefs["OneHeroTRUE"] + coefs["ScenarioSandman:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioSandman:DifficultyLevelExpert"] + coefs["ScenarioSandman:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioVenom"] + coefs["OneHeroTRUE"] + coefs["ScenarioVenom:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioVenom:DifficultyLevelExpert"] + coefs["ScenarioVenom:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioMysterio"] + coefs["OneHeroTRUE"] + coefs["ScenarioMysterio:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioMysterio:DifficultyLevelExpert"] + coefs["ScenarioMysterio:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioThe Sinister Six"] + coefs["OneHeroTRUE"] + coefs["ScenarioThe Sinister Six:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioThe Sinister Six:DifficultyLevelExpert"] + coefs["ScenarioThe Sinister Six:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"],
			coefs["ScenarioVenom Goblin"] + coefs["OneHeroTRUE"] + coefs["ScenarioVenom Goblin:OneHeroTRUE"] + coefs["DifficultyLevelExpert"] + 
				coefs["ScenarioVenom Goblin:DifficultyLevelExpert"] + coefs["ScenarioVenom Goblin:DifficultyLevelExpert:OneHeroTRUE"] +
				coefs["DifficultyLevelExpert:OneHeroTRUE"]
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
				"Venom Goblin"
			),
			Standard = round(standard * 10),
			Expert = round(expert * 10),
			Standard_One = round(standardOneHero * 10),
			Expert_One = round(expertOneHero * 10)
		)
	)
}


MarvelEncounterSets <- function(marvelGlm) {
	coefs <- coef(marvelGlm)
	return(data.frame(
		row.names = c(
		    " - Core Set - ",
			"Bomb Scare",
			"Masters of Evil",
			"Under Attack",
			"Legions of Hydra",
			"Doomsday Chair",
			" - Green Goblin - ",
			"Goblin Gimmicks",
			"Mess of Things",
			"Power Drain",
			"Running Interference",
			"Kree Fanatic",
			" - Rise of Red Skull - ",
			"Experimental Weapons",
			"Hydra Assault",
			"Hydra Patrol",
			"Weapon Master",
			" - Kang - ",
			"Temporal",
			"Master of Time",
			"Anachronauts",
			" - Galaxy's Most Wanted - ",
			"Band of Badoon",
			"Menagerie Medley",
			"Galactic Artifacts",
			"Space Pirates",
			"Kree Militants",
			"Ship Command",
			"Badoon Headhunter",
			" - Mad Titan's Shadow - ",
			"Black Order",
			"Armies of Titan",
			"Children of Thanos",
			"Legions of Hel",
			"Frost Giants",
			"Enchantress",
			"Infinity Gauntlet",
			" - The Hood - ",
			"Beasty Boys",
			"Brothers Grimm",
			"Crossfire's Crew",
			"Mister Hyde",
			"Ransacked Armory",
			"Sinister Syndicate",
			"State of Emergency",
			"Streets of Mayhem",
			"Wrecking Crew",
			" - Sinister Motives - ",
			"City in Chaos",
			"Down to Earth",
			"Goblin Gear",
			"Guerrilla Tactics",
			"Osborn Tech",
			"Personal Nightmare",
			"Symbiotic Strength",
			"Whispers of Paranoia",
			"",
			"Heroic",
			"Skirmish",
			"Standard II",
			"Expert II"
		),
		Ratings = c(
		    "",
			sprintf("%+d", round(coefs["BombScareTRUE"] * 10)),
			sprintf("%+d", round(coefs["MastersOfEvilTRUE"] * 10)),
			sprintf("%+d", round(coefs["UnderAttackTRUE"] * 10)),
			sprintf("%+d", round(coefs["LegionsOfHydraTRUE"] * 10)),
			sprintf("%+d", round(coefs["DoomsdayChairTRUE"] * 10)),
			"",
			sprintf("%+d", round(coefs["GoblinGimmicksTRUE"] * 10)),
			sprintf("%+d", round(coefs["MessOfThingsTRUE"] * 10)),
			sprintf("%+d", round(coefs["PowerDrainTRUE"] * 10)),
			sprintf("%+d", round(coefs["RunningInterferenceTRUE"] * 10)),
			sprintf("%+d", round(coefs["KreeFanaticTRUE"] * 10)),
			"",
			sprintf("%+d", round(coefs["ExperimentalWeaponsTRUE"] * 10)),
			sprintf("%+d", round(coefs["HydraAssaultTRUE"] * 10)),
			sprintf("%+d", round(coefs["HydraPatrolTRUE"] * 10)),
			sprintf("%+d", round(coefs["WeaponMasterTRUE"] * 10)),
			"",
			sprintf("%+d", round(coefs["TemporalTRUE"] * 10)),
			sprintf("%+d", round(coefs["MasterOfTimeTRUE"] * 10)),
			sprintf("%+d", round(coefs["AnachronautsTRUE"] * 10)),
			"",
			sprintf("%+d", round(coefs["BandOfBadoonTRUE"] * 10)),
			sprintf("%+d", round(coefs["MenagerieMedleyTRUE"] * 10)),
			sprintf("%+d", round(coefs["GalacticArtifactsTRUE"] * 10)),
			sprintf("%+d", round(coefs["SpacePiratesTRUE"] * 10)),
			sprintf("%+d", round(coefs["KreeMilitantsTRUE"] * 10)),
			sprintf("%+d", round(coefs["ShipCommandTRUE"] * 10)),
			sprintf("%+d", round(coefs["BadoonHeadhunterTRUE"] * 10)),
			"",
			sprintf("%+d", round(coefs["BlackOrderTRUE"] * 10)),
			sprintf("%+d", round(coefs["ArmiesOfTitanTRUE"] * 10)),
			sprintf("%+d", round(coefs["ChildrenOfThanosTRUE"] * 10)),
			sprintf("%+d", round(coefs["LegionsOfHelTRUE"] * 10)),
			sprintf("%+d", round(coefs["FrostGiantsTRUE"] * 10)),
			sprintf("%+d", round(coefs["EnchantressTRUE"] * 10)),
			sprintf("%+d", round(coefs["InfinityGauntletTRUE"] * 10)),
			"",
			sprintf("%+d", round(coefs["BeastyBoysTRUE"] * 10)),
			sprintf("%+d", round(coefs["BrothersGrimmTRUE"] * 10)),
			sprintf("%+d", round(coefs["CrossfiresCrewTRUE"] * 10)),
			sprintf("%+d", round(coefs["MisterHydeTRUE"] * 10)),
			sprintf("%+d", round(coefs["RansackedArmoryTRUE"] * 10)),
			sprintf("%+d", round(coefs["SinisterSyndicateTRUE"] * 10)),
			sprintf("%+d", round(coefs["StateOfEmergencyTRUE"] * 10)),
			sprintf("%+d", round(coefs["StreetsOfMayhemTRUE"] * 10)),
			sprintf("%+d", round(coefs["WreckingCrewTRUE"] * 10)),
			"",
			sprintf("%+d", round(coefs["CityInChaosTRUE"] * 10)),
			sprintf("%+d", round(coefs["DownToEarthTRUE"] * 10)),
			sprintf("%+d", round(coefs["GoblinGearTRUE"] * 10)),
			sprintf("%+d", round(coefs["GuerrillaTacticsTRUE"] * 10)),
			sprintf("%+d", round(coefs["OsbornTechTRUE"] * 10)),
			sprintf("%+d", round(coefs["PersonalNightmareTRUE"] * 10)),
			sprintf("%+d", round(coefs["SymbioticStrengthTRUE"] * 10)),
			sprintf("%+d", round(coefs["WhispersOfParanoiaTRUE"] * 10)),
            "",
			sprintf("%+d", round(coefs["Heroic"] * 10)),
			sprintf("%+d", round(coefs["SkirmishTRUE"] * 10)),
			sprintf("%+d", round(coefs["Standard2TRUE"] * 10)),
			sprintf("%+d", round(coefs["Expert2TRUE"] * 10))
        )
	))
}

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
			"Quicksilver",
			"Rocket Raccoon",
			"Scarlet Witch",
			"She-Hulk",
			"Spectrum",
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
			sprintf("%+d", round(coefs["QuicksilverTRUE"] * 10)),
			sprintf("%+d", round(coefs["RocketRaccoonTRUE"] * 10)),
			sprintf("%+d", round(coefs["ScarletWitchTRUE"] * 10)),
			sprintf("%+d", round(coefs["SheHulkTRUE"] * 10)),
			sprintf("%+d", round(coefs["SpectrumTRUE"] * 10)),
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

MarvelStars <- function(marvelGlm, coefEasiestScenario) {
	stars <- seq(0, 5, by=.5)
	easiest <- data.frame(
        Timestamp = Sys.time(),
		Scenario = "Rhino",
		DifficultyLevel = "Standard",
		OneHero = FALSE,
		Undeclared = FALSE,
		BombScare = TRUE, MastersOfEvil = FALSE, UnderAttack = FALSE, LegionsOfHydra = FALSE, DoomsdayChair = FALSE,
		GoblinGimmicks = FALSE, MessOfThings = FALSE, PowerDrain = FALSE, RunningInterference = FALSE,
		KreeFanatic = FALSE,
		ExperimentalWeapons = FALSE, HydraAssault = FALSE, HydraPatrol = FALSE, WeaponMaster = FALSE,
		Temporal = FALSE, MasterOfTime = FALSE, Anachronauts = FALSE,
		BandOfBadoon = FALSE, MenagerieMedley = FALSE, GalacticArtifacts = FALSE, SpacePirates = FALSE, KreeMilitants = FALSE,
		BadoonHeadhunter = FALSE, ShipCommand = FALSE,
		BlackOrder = FALSE, ArmiesOfTitan = FALSE, ChildrenOfThanos = FALSE, LegionsOfHel = FALSE, FrostGiants = FALSE, Enchantress = FALSE, InfinityGauntlet = FALSE,
		BeastyBoys = FALSE, BrothersGrimm = FALSE, CrossfiresCrew = FALSE, MisterHyde = FALSE,
		RansackedArmory = FALSE, SinisterSyndicate = FALSE, StateOfEmergency = FALSE,
		StreetsOfMayhem = FALSE, WreckingCrew = FALSE,
		CityInChaos = FALSE, DownToEarth = FALSE, GoblinGear = FALSE, GuerrillaTactics = FALSE, OsbornTech = FALSE,
		PersonalNightmare = FALSE, SinisterAssault = FALSE, SymbioticStrength = FALSE, WhispersOfParanoia = FALSE,
		Heroic = 0, SkirmishLevel = 0, Standard2 = FALSE, Expert2 = FALSE,
		CampaignAbsorbingMan = FALSE, CampaignTaskmaster = FALSE, CampaignZola = FALSE, CampaignRedSkull = FALSE,
		CampaignBrotherhood = FALSE, CampaignInfiltrateMuseum = FALSE, CampaignEscapeMuseum = FALSE, CampaignNebula = FALSE, CampaignRonan = FALSE,
		CampaignEbonyMaw = FALSE, CampaignTowerDefense = FALSE, CampaignThanos = FALSE, CampaignHela = FALSE, CampaignLoki = FALSE,
		CampaignSandman = FALSE, CampaignVenom = FALSE, CampaignMysterio = FALSE, CampaignSinisterSix = FALSE, CampaignVenomGoblin = FALSE,
		Aggression = FALSE, Justice = FALSE, Leadership = FALSE, Protection = FALSE,
		AdamWarlock = FALSE, AntMan = FALSE, BlackPanther = FALSE, BlackWidow = FALSE,
		CaptainAmerica = FALSE, CaptainMarvel = FALSE,
		DoctorStrange = FALSE, Drax = FALSE, Gamora = FALSE, GhostSpider = FALSE, Groot = FALSE,
		Hawkeye = FALSE, Hulk = FALSE, IronMan = FALSE, Ironheart = FALSE, Nebula = FALSE, Nova = FALSE,
		MsMarvel = FALSE, Quicksilver = FALSE,
		RocketRaccoon = FALSE, ScarletWitch = FALSE, SheHulk = FALSE,
		Spectrum = FALSE, SpiderMan = FALSE, SpiderManMilesMorales = FALSE, SpiderWoman = FALSE, StarLord = FALSE,
		Thor = FALSE, Valkyrie = FALSE, Vision = FALSE, WarMachine = FALSE, Wasp = FALSE, Venom = FALSE,
		AdamWarlockSolo = FALSE, AntManSolo = FALSE, BlackPantherSolo = FALSE, BlackWidowSolo = FALSE,
		CaptainAmericaSolo = FALSE, CaptainMarvelSolo = FALSE,
		DoctorStrangeSolo = FALSE, DraxSolo = FALSE, GamoraSolo = FALSE, GhostSpiderSolo = FALSE, GrootSolo = FALSE,
		HawkeyeSolo = FALSE, HulkSolo = FALSE, IronManSolo = FALSE, IronheartSolo = FALSE, NebulaSolo = FALSE, NovaSolo = FALSE,
		MsMarvelSolo = FALSE, QuicksilverSolo = FALSE,
		RocketRaccoonSolo = FALSE, ScarletWitchSolo = FALSE, SheHulkSolo = FALSE,
		SpectrumSolo = FALSE, SpiderManSolo = FALSE, SpiderManMilesMoralesSolo = FALSE, SpiderWomanSolo = FALSE, StarLordSolo = FALSE,
		ThorSolo = FALSE, ValkyrieSolo = FALSE, VisionSolo = FALSE, WarMachineSolo = FALSE, WaspSolo = FALSE, VenomSolo = FALSE
		)
	hardest <- data.frame(
        Timestamp = Sys.time(),
		Scenario = "Ultron",
		DifficultyLevel = "Expert",
		OneHero = FALSE,
		Undeclared = FALSE,
		BombScare = FALSE, MastersOfEvil = FALSE, UnderAttack = FALSE, LegionsOfHydra = FALSE, DoomsdayChair = TRUE,
		GoblinGimmicks = FALSE, MessOfThings = FALSE, PowerDrain = FALSE, RunningInterference = FALSE,
		KreeFanatic = FALSE,
		ExperimentalWeapons = FALSE, HydraAssault = FALSE, HydraPatrol = FALSE, WeaponMaster = FALSE,
		Temporal = FALSE, MasterOfTime = FALSE, Anachronauts = FALSE,
		BandOfBadoon = FALSE, MenagerieMedley = FALSE, GalacticArtifacts = FALSE, SpacePirates = FALSE, KreeMilitants = FALSE,
		BadoonHeadhunter = FALSE, ShipCommand = FALSE,
		BlackOrder = FALSE, ArmiesOfTitan = FALSE, ChildrenOfThanos = FALSE, LegionsOfHel = FALSE, FrostGiants = FALSE, Enchantress = FALSE, InfinityGauntlet = FALSE,
		BeastyBoys = FALSE, BrothersGrimm = FALSE, CrossfiresCrew = FALSE, MisterHyde = FALSE,
		RansackedArmory = FALSE, SinisterSyndicate = FALSE, StateOfEmergency = FALSE,
		StreetsOfMayhem = FALSE, WreckingCrew = FALSE,
		CityInChaos = FALSE, DownToEarth = FALSE, GoblinGear = FALSE, GuerrillaTactics = FALSE, OsbornTech = FALSE,
		PersonalNightmare = FALSE, SinisterAssault = FALSE, SymbioticStrength = FALSE, WhispersOfParanoia = FALSE,
		Heroic = 1, SkirmishLevel = 0, Standard2 = FALSE, Expert2 = FALSE,
		CampaignAbsorbingMan = FALSE, CampaignTaskmaster = FALSE, CampaignZola = FALSE, CampaignRedSkull = FALSE,
		CampaignBrotherhood = FALSE, CampaignInfiltrateMuseum = FALSE, CampaignEscapeMuseum = FALSE, CampaignNebula = FALSE, CampaignRonan = FALSE,
		CampaignEbonyMaw = FALSE, CampaignTowerDefense = FALSE, CampaignThanos = FALSE, CampaignHela = FALSE, CampaignLoki = FALSE,
		CampaignSandman = FALSE, CampaignVenom = FALSE, CampaignMysterio = FALSE, CampaignSinisterSix = FALSE, CampaignVenomGoblin = FALSE,
		Aggression = FALSE, Justice = FALSE, Leadership = FALSE, Protection = FALSE,
		AdamWarlock = FALSE, AntMan = FALSE, BlackPanther = FALSE, BlackWidow = FALSE,
		CaptainAmerica = FALSE, CaptainMarvel = FALSE,
		DoctorStrange = FALSE, Drax = FALSE, Gamora = FALSE, GhostSpider = FALSE, Groot = FALSE,
		Hawkeye = FALSE, Hulk = FALSE, IronMan = FALSE, Ironheart = FALSE, Nebula = FALSE, Nova = FALSE,
		MsMarvel = FALSE, Quicksilver = FALSE,
		RocketRaccoon = FALSE, ScarletWitch = FALSE, SheHulk = FALSE,
		Spectrum = FALSE, SpiderMan = FALSE, SpiderManMilesMorales = FALSE, SpiderWoman = FALSE, StarLord = FALSE,
		Thor = FALSE, Valkyrie = FALSE, Vision = FALSE, WarMachine = FALSE, Wasp = FALSE, Venom = FALSE,
		AdamWarlockSolo = FALSE, AntManSolo = FALSE, BlackPantherSolo = FALSE, BlackWidowSolo = FALSE,
		CaptainAmericaSolo = FALSE, CaptainMarvelSolo = FALSE,
		DoctorStrangeSolo = FALSE, DraxSolo = FALSE, GamoraSolo = FALSE, GhostSpiderSolo = FALSE, GrootSolo = FALSE,
		HawkeyeSolo = FALSE, HulkSolo = FALSE, IronManSolo = FALSE, IronheartSolo = FALSE, NebulaSolo = FALSE, NovaSolo = FALSE,
		MsMarvelSolo = FALSE, QuicksilverSolo = FALSE,
		RocketRaccoonSolo = FALSE, ScarletWitchSolo = FALSE, SheHulkSolo = FALSE,
		SpectrumSolo = FALSE, SpiderManSolo = FALSE, SpiderManMilesMoralesSolo = FALSE, SpiderWomanSolo = FALSE, StarLordSolo = FALSE,
		ThorSolo = FALSE, ValkyrieSolo = FALSE, VisionSolo = FALSE, WarMachineSolo = FALSE, WaspSolo = FALSE, VenomSolo = FALSE
		)
	predRange <- predict.glm(marvelGlm, newdata=hardest) - predict.glm(marvelGlm, newdata=easiest)
	# 1.4 is about 80% chance of losing, -2.0 is about 12% chance of losing
	# predRange <- 1.4 - -2.0
	logits <- seq(predict.glm(marvelGlm, newdata=easiest), predict.glm(marvelGlm, newdata=hardest), predRange / 10)
	# logits <- seq(-2.0, 1.4, predRange / 10)
	# maybe this should be intercept?
	logitMinusEasiest <- logits - coefEasiestScenario
	# The BreakPoint means "any value higher than this is the next level of stars"
	# I.e., 
	#    Stars BreakPoint
    # 1    0.0      -4.67 -- anything higher than -4.7 is 0.5 stars
    # 2    0.5      -0.86 -- anything higher than -0.9 is 1 star
    # 3    1.0       2.95 -- anything higher than 2.9 is 1.5 stars
	return(data.frame(
		Stars=stars, 
		BreakPoint=round(logitMinusEasiest * 10, 1),
		Low=c(NA, ceiling(logitMinusEasiest[1:10] * 10)),
		High=floor(logitMinusEasiest * 10)
	))
}
