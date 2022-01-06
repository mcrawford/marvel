library(anytime)
library(googlesheets4)

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

	for (i in 1:10) {
		for (s in levels(marvel$Scenario)) {
		    standardWin <- if (i <= 8) "Yes" else "No"
		    expertWin <- if (i <= 2) "Yes" else "No"
			multiStandard <- data.frame(
				Timestamp = Sys.time(),
				First = "", FirstAspect = "", IsSecondHero = "Yes", FirstTwoAspects ="", IsSecondHero1 = "No", IsSecondHero2 = "No", 
				Second = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
				Third = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
				Fourth = "", FourthAspect = "", FourthTwoAspects = "",
				Scenario = s, 
				Campaign = "No", ExpertCampaign = "No", Encounter = "", 
				Win = standardWin, Standard = "Standard (Core Set)", Expert = "", Heroic = 0, Skirmish = 0,
				WinWC = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Standard"
			)
			multiExpert <- data.frame(
				Timestamp = Sys.time(),
				First = "", FirstAspect = "", IsSecondHero = "Yes", FirstTwoAspects ="", IsSecondHero1 = "No", IsSecondHero2 = "No", 
				Second = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
				Third = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
				Fourth = "", FourthAspect = "", FourthTwoAspects = "",
				Scenario = s, 
				Campaign = "No", ExpertCampaign = "No", Encounter = "", 
				Win = expertWin, Standard = "Standard (Core Set)", Expert = "Expert (Core Set)", Heroic = 0, Skirmish = 0,
				WinWC = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Expert"
			)
			soloStandard <- data.frame(
				Timestamp = Sys.time(),
				First = "", FirstAspect = "", IsSecondHero = "No", FirstTwoAspects ="", IsSecondHero1 = "No", IsSecondHero2 = "No", 
				Second = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
				Third = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
				Fourth = "", FourthAspect = "", FourthTwoAspects = "",
				Scenario = s, 
				Campaign = "No", ExpertCampaign = "No", Encounter = "", 
				Win = standardWin, Standard = "Standard (Core Set)", Expert = "", Heroic = 0, Skirmish = 0,
				WinWC = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Standard"
			)
			soloExpert <- data.frame(
				Timestamp = Sys.time(),
				First = "", FirstAspect = "", IsSecondHero = "No", FirstTwoAspects ="", IsSecondHero1 = "No", IsSecondHero2 = "No", 
				Second = "", SecondAspect = "", IsThirdHero = "No", SecondTwoAspects = "", IsThirdHero1 = "No", IsThirdHero2 = "No",
				Third = "", ThirdAspect = "", IsFourthHero = "No", ThirdTwoAspects = "", IsFourthHero1 = "No", IsFourthHero2 = "No",
				Fourth = "", FourthAspect = "", FourthTwoAspects = "",
				Scenario = s, 
				Campaign = "No", ExpertCampaign = "No", Encounter = "", 
				Win = expertWin, Standard = "Standard (Core Set)", Expert = "Expert (Core Set)", Heroic = 0, Skirmish = 0,
				WinWC = "", DifficultyWC = "", HeroicWC = 0, Difficulty = "Expert"
			)
			marvel <- rbind(marvel, multiStandard)
			marvel <- rbind(marvel, multiExpert)
			marvel <- rbind(marvel, soloStandard)
			marvel <- rbind(marvel, soloExpert)
		}
	}

	marvel$DifficultyLevel = ""
	marvel$DifficultyLevel[grepl("Standard", marvel$Standard, fixed=TRUE)] <- "Standard"
	marvel$DifficultyLevel[grepl("Expert", marvel$Expert, fixed=TRUE)] <- "Expert"
	marvel$DifficultyLevel[grepl("Standard", marvel$DifficultyWC, fixed=TRUE)] <- "Standard"
	marvel$DifficultyLevel[grepl("Expert", marvel$DifficultyWC, fixed=TRUE)] <- "Expert"
	marvel$DifficultyLevel[grepl("Extreme", marvel$DifficultyWC, fixed=TRUE)] <- "Expert"
	marvel$DifficultyLevel <- relevel(factor(marvel$DifficultyLevel), ref="Standard")

	marvel$Heroic[is.na(marvel$Heroic)] <- 0
	marvel$Heroic[marvel$Heroic > 3] <- 3
	marvel$OneHero <- (marvel$IsSecondHero == "No")
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
	marvel$BadoonHeadhunter <- grepl("Badoon Headhunter", marvel$Encounter, fixed=TRUE)
	marvel$BlackOrder <- grepl("Black Order", marvel$Encounter, fixed=TRUE)
	marvel$ArmiesOfTitan <- grepl("Armies of Titan", marvel$Encounter, fixed=TRUE)
	marvel$ChildrenOfThanos <- grepl("Children of Thanos", marvel$Encounter, fixed=TRUE)
	marvel$LegionsOfHel <- grepl("Legions of Hel", marvel$Encounter, fixed=TRUE)
	marvel$FrostGiants <- grepl("Frost Giants", marvel$Encounter, fixed=TRUE)
    marvel$Enchantress <- grepl("Enchantress", marvel$Encounter, fixed=TRUE)

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
	marvel$Groot <- marvel$First == "Groot" | marvel$Second == "Groot" | 
						 marvel$Third == "Groot" | marvel$Fourth == "Groot"
	marvel$Hawkeye <- marvel$First == "Hawkeye" | marvel$Second == "Hawkeye" | 
						 marvel$Third == "Hawkeye" | marvel$Fourth == "Hawkeye"
	marvel$Hulk <- marvel$First == "Hulk" | marvel$Second == "Hulk" | 
						 marvel$Third == "Hulk" | marvel$Fourth == "Hulk"
	marvel$IronMan <- marvel$First == "Iron Man" | marvel$Second == "Iron Man" | 
						 marvel$Third == "Iron Man" | marvel$Fourth == "Iron Man"
	marvel$MsMarvel <- marvel$First == "Ms. Marvel" | marvel$Second == "Ms. Marvel" | 
						 marvel$Third == "Ms. Marvel" | marvel$Fourth == "Ms. Marvel"
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
	marvel$SpiderMan <- marvel$First == "Spider-Man" | marvel$Second == "Spider-Man" | 
						 marvel$Third == "Spider-Man" | marvel$Fourth == "Spider-Man"
	marvel$SpiderWoman <- marvel$First == "Spider-Woman" | marvel$Second == "Spider-Woman" | 
						 marvel$Third == "Spider-Woman" | marvel$Fourth == "Spider-Woman"
	marvel$StarLord <- marvel$First == "Star-Lord" | marvel$Second == "Star-Lord" | 
						 marvel$Third == "Star-Lord" | marvel$Fourth == "Star-Lord"
	marvel$Thor <- marvel$First == "Thor" | marvel$Second == "Thor" | 
						 marvel$Third == "Thor" | marvel$Fourth == "Thor"
	marvel$Venom <- marvel$First == "Venom" | marvel$Second == "Venom" | 
						 marvel$Third == "Venom" | marvel$Fourth == "Venom"
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
	marvel$GrootSolo <- marvel$Groot & marvel$OneHero
	marvel$HawkeyeSolo <- marvel$Hawkeye & marvel$OneHero
	marvel$HulkSolo <- marvel$Hulk & marvel$OneHero
	marvel$IronManSolo <- marvel$IronMan & marvel$OneHero
	marvel$MsMarvelSolo <- marvel$MsMarvel & marvel$OneHero
	marvel$QuicksilverSolo <- marvel$Quicksilver & marvel$OneHero
	marvel$RocketRaccoonSolo <- marvel$RocketRaccoon & marvel$OneHero
	marvel$ScarletWitchSolo <- marvel$ScarletWitch & marvel$OneHero
	marvel$SheHulkSolo <- marvel$SheHulk & marvel$OneHero
	marvel$SpectrumSolo <- marvel$Spectrum & marvel$OneHero
	marvel$SpiderManSolo <- marvel$SpiderMan & marvel$OneHero
	marvel$SpiderWomanSolo <- marvel$SpiderWoman & marvel$OneHero
	marvel$StarLordSolo <- marvel$StarLord & marvel$OneHero
	marvel$ThorSolo <- marvel$Thor & marvel$OneHero
	marvel$WaspSolo <- marvel$Wasp & marvel$OneHero
	marvel$VenomSolo <- marvel$Venom & marvel$OneHero

	return(marvel)
}

MarvelGlm <- function(marvel) {
	return(glm(formula = (Win == "No") ~ 
		Scenario * DifficultyLevel * OneHero + 
		Timestamp +
		Undeclared +
    	BombScare + 
    	MastersOfEvil + 
    	UnderAttack + 
    	LegionsOfHydra + 
    	DoomsdayChair + 
    	GoblinGimmicks + 
    	MessOfThings + 
    	PowerDrain + 
    	RunningInterference + 
    	KreeFanatic +
		ExperimentalWeapons +
		HydraAssault +
		HydraPatrol +
		WeaponMaster +
		Temporal +
		MasterOfTime +
		Anachronauts +
		BandOfBadoon + 
		MenagerieMedley +
		GalacticArtifacts +
		SpacePirates +
		KreeMilitants +
		BadoonHeadhunter +
		BlackOrder +
		ArmiesOfTitan +
		ChildrenOfThanos +
		LegionsOfHel +
		FrostGiants + 
		Enchantress +
    	Heroic +
		CampaignAbsorbingMan + CampaignTaskmaster + CampaignZola + CampaignRedSkull +
		CampaignBrotherhood + CampaignInfiltrateMuseum + CampaignEscapeMuseum + CampaignNebula + CampaignRonan +
		CampaignEbonyMaw + CampaignTowerDefense + CampaignThanos + CampaignHela + CampaignLoki +
		Aggression + Justice + Leadership + Protection +
		AdamWarlock + AntMan + BlackPanther + 
		BlackWidow + CaptainAmerica + CaptainMarvel +
		DoctorStrange + Drax + Gamora + Groot + 
		Hawkeye + Hulk + IronMan + 
		MsMarvel + Quicksilver +
		RocketRaccoon + ScarletWitch + SheHulk + Spectrum + 
		SpiderMan + SpiderWoman + StarLord +
		Thor + Wasp + Venom +
		AdamWarlockSolo + AntManSolo + BlackPantherSolo + 
		BlackWidowSolo + CaptainAmericaSolo + CaptainMarvelSolo +
		DoctorStrangeSolo + DraxSolo + GamoraSolo + GrootSolo + 
		HawkeyeSolo + HulkSolo + IronManSolo + 
		MsMarvelSolo + QuicksilverSolo +
		RocketRaccoonSolo + ScarletWitchSolo + SheHulkSolo + SpectrumSolo + 
		SpiderManSolo + SpiderWomanSolo + StarLordSolo +
		ThorSolo + WaspSolo + VenomSolo
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
			coefs["ScenarioLoki"]
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
			coefs["ScenarioHela"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioHeloa:DifficultyLevelExpert"],
			coefs["ScenarioLoki"] + coefs["DifficultyLevelExpert"] + coefs["ScenarioLoki:DifficultyLevelExpert"]
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
			coefs["ScenarioLoki"] + coefs["OneHeroTRUE"] + coefs["ScenarioLoki:OneHeroTRUE"]
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
				"Loki"
			),
			Standard = round(standard * 10),
			Expert = round(expert * 10),
			Standard_One = round(standardOneHero * 10),
			Expert_One = round(expertOneHero * 10)
		)
	)
}

UpdateGoogleSheet <- function(marvelGlm) {
	coefs <- coef(marvelGlm)
	marvelFactors <- MarvelFactors(marvelGlm)
	range_write("1Ju_R8SD41r1dPjded__PnMauAdtkCsr388kGSbp80_w", marvelFactors[, 1:2], "Villains", "I3", F)
	range_write("1Ju_R8SD41r1dPjded__PnMauAdtkCsr388kGSbp80_w", marvelFactors[, 3:4], "Villains", "D3", F)
	range_write("1Ju_R8SD41r1dPjded__PnMauAdtkCsr388kGSbp80_w", data.frame(Heroic=round(coefs["Heroic"] * 10)), "Villains", "N1")
	encounters <- data.frame(
		row.names = c(
			"Bomb Scare",
			"Masters of Evil",
			"Under Attack",
			"Legions of Hydra",
			"Doomsday Chair",
			"Goblin Gimmicks",
			"Mess of Things",
			"Power Drain",
			"Running Interference",
			"Kree Fanatic",
			"Experimental Weapons",
			"Hydra Assault",
			"Hydra Patrol",
			"Weapon Master",
			"Temporal",
			"Master of Time",
			"Anachronauts",
			"Band of Badoon",
			"Menagerie Medley",
			"Galactic Artifacts",
			"Space Pirates",
			"Kree Militants",
			"Badoon Headhunter",
			"Black Order",
			"Armies of Titan",
			"Children of Thanos",
			"Legions of Hel",
			"Frost Giants",
			"Enchantress"
		),
		Rating = round(c(
			coefs["BombScareTRUE"],
			coefs["MastersOfEvilTRUE"],
			coefs["UnderAttackTRUE"],
			coefs["LegionsOfHydraTRUE"],
			coefs["DoomsdayChairTRUE"],
			coefs["GoblinGimmicksTRUE"],
			coefs["MessOfThingsTRUE"],
			coefs["PowerDrainTRUE"],
			coefs["RunningInterferenceTRUE"],
			coefs["KreeFanaticTRUE"],
			coefs["ExperimentalWeaponsTRUE"],
			coefs["HydraAssaultTRUE"],
			coefs["HydraPatrolTRUE"],
			coefs["WeaponMasterTRUE"],
			coefs["TemporalTRUE"],
			coefs["MasterOfTimeTRUE"],
			coefs["AnachronautsTRUE"],
			coefs["BandOfBadoonTRUE"],
			coefs["MenagerieMedleyTRUE"],
			coefs["GalacticArtifactsTRUE"],
			coefs["SpacePiratesTRUE"],
			coefs["KreeMilitantsTRUE"],
			coefs["BadoonHeadhunterTRUE"],
			coefs["BlackOrderTRUE"],
			coefs["ArmiesOfTitanTRUE"],
			coefs["ChildrenOfThanosTRUE"],
			coefs["LegionsOfHelTRUE"],
			coefs["FrostGiantsTRUE"],
			coefs["EnchantressTRUE"]
		) * 10)
	)
	range_write("1Ju_R8SD41r1dPjded__PnMauAdtkCsr388kGSbp80_w", encounters, "Encounter Sets", "B1")
}

MarvelEncounterSets <- function(marvelGlm) {
	coefs <- coef(marvelGlm)
	return(data.frame(
		row.names = c(
			"Bomb Scare",
			"Masters of Evil",
			"Under Attack",
			"Legions of Hydra",
			"Doomsday Chair",
			"Goblin Gimmicks",
			"Mess of Things",
			"Power Drain",
			"Running Interference",
			"Kree Fanatic",
			"Experimental Weapons",
			"Hydra Assault",
			"Hydra Patrol",
			"Weapon Master",
			"Temporal",
			"Master of Time",
			"Anachronauts",
			"Band of Badoon",
			"Menagerie Medley",
			"Galactic Artifacts",
			"Space Pirates",
			"Kree Militants",
			"Badoon Headhunter",
			"Black Order",
			"Armies of Titan",
			"Children of Thanos",
			"Legions of Hel",
			"Frost Giants",
			"Enchantress",
			"Heroic"
		),
		Ratings = sprintf("%+d", round(c(
			coefs["BombScareTRUE"],
			coefs["MastersOfEvilTRUE"],
			coefs["UnderAttackTRUE"],
			coefs["LegionsOfHydraTRUE"],
			coefs["DoomsdayChairTRUE"],
			coefs["GoblinGimmicksTRUE"],
			coefs["MessOfThingsTRUE"],
			coefs["PowerDrainTRUE"],
			coefs["RunningInterferenceTRUE"],
			coefs["KreeFanaticTRUE"],
			coefs["ExperimentalWeaponsTRUE"],
			coefs["HydraAssaultTRUE"],
			coefs["HydraPatrolTRUE"],
			coefs["WeaponMasterTRUE"],
			coefs["TemporalTRUE"],
			coefs["MasterOfTimeTRUE"],
			coefs["AnachronautsTRUE"],
			coefs["BandOfBadoonTRUE"],
			coefs["MenagerieMedleyTRUE"],
			coefs["GalacticArtifactsTRUE"],
			coefs["SpacePiratesTRUE"],
			coefs["KreeMilitantsTRUE"],
			coefs["BadoonHeadhunterTRUE"],
			coefs["BlackOrderTRUE"],
			coefs["ArmiesOfTitanTRUE"],
			coefs["ChildrenOfThanosTRUE"],
			coefs["LegionsOfHelTRUE"],
			coefs["FrostGiantsTRUE"],
			coefs["EnchantressTRUE"],
			coefs["Heroic"]
		) * 10))
	))
}

# Not updated yet for MTS
MarvelStars <- function(marvelGlm, coefEasiestScenario) {
	stars <- seq(0, 5, by=.5)
	easiest <- data.frame(
		Scenario = "Rhino",
		DifficultyLevel = "Standard",
		OneHero = FALSE,
		Timestamp = Sys.time(),
		Undeclared = FALSE,
		BombScare = TRUE,
		MastersOfEvil = FALSE,
		UnderAttack = FALSE,
		LegionsOfHydra = FALSE,
		DoomsdayChair = FALSE,
		GoblinGimmicks = FALSE,
		MessOfThings = FALSE,
		PowerDrain = FALSE,
		RunningInterference = FALSE,
		KreeFanatic = FALSE,
		ExperimentalWeapons = FALSE,
		HydraAssault = FALSE,
		HydraPatrol = FALSE,
		WeaponMaster = FALSE,
		Temporal = FALSE,
		MasterOfTime = FALSE,
		Anachronauts = FALSE,
		BandOfBadoon = FALSE,
		MenagerieMedley = FALSE,
		GalacticArtifacts = FALSE,
		SpacePirates = FALSE,
		KreeMilitants = FALSE,
		BadoonHeadhunter = FALSE,
		Heroic = 0,
		CampaignAbsorbingMan = FALSE, CampaignTaskmaster = FALSE, CampaignZola = FALSE, CampaignRedSkull = FALSE,
		CampaignBrotherhood = FALSE, CampaignInfiltrateMuseum = FALSE, CampaignEscapeMuseum = FALSE, CampaignNebula = FALSE, CampaignRonan = FALSE,
		Aggression = FALSE, Justice = FALSE, Leadership = FALSE, Protection = FALSE,
		AntMan = FALSE, BlackPanther = FALSE, BlackWidow = FALSE, CaptainAmerica = FALSE, CaptainMarvel = FALSE,
		DoctorStrange = FALSE, Drax = FALSE, Gamora = FALSE, Groot = FALSE, Hawkeye = FALSE, Hulk = FALSE, IronMan = FALSE, 
		MsMarvel = FALSE, Quicksilver = FALSE,
		RocketRaccoon = FALSE, ScarletWitch = FALSE, SheHulk = FALSE, SpiderMan = FALSE, SpiderWoman = FALSE, StarLord = FALSE, 
		Thor = FALSE, Wasp = FALSE, Venom = FALSE, 
		AntManSolo = FALSE, BlackPantherSolo = FALSE, BlackWidowSolo = FALSE, CaptainAmericaSolo = FALSE, CaptainMarvelSolo = FALSE,
		DoctorStrangeSolo = FALSE, DraxSolo = FALSE, GamoraSolo = FALSE, GrootSolo = FALSE, HawkeyeSolo = FALSE, HulkSolo = FALSE, IronManSolo = FALSE, 
		MsMarvelSolo = FALSE, QuicksilverSolo = FALSE,
		RocketRaccoonSolo = FALSE, ScarletWitchSolo = FALSE, SheHulkSolo = FALSE, SpiderManSolo = FALSE, SpiderWomanSolo = FALSE, StarLordSolo = FALSE,
		ThorSolo = FALSE, WaspSolo = FALSE, VenomSolo = FALSE
		)
	hardest <- data.frame(
		Scenario = "Ultron",
		DifficultyLevel = "Expert",
		OneHero = FALSE,
		Timestamp = Sys.time(),
		Undeclared = FALSE,
		BombScare = FALSE,
		MastersOfEvil = FALSE,
		UnderAttack = FALSE,
		LegionsOfHydra = FALSE,
		DoomsdayChair = TRUE,
		GoblinGimmicks = FALSE,
		MessOfThings = FALSE,
		PowerDrain = FALSE,
		RunningInterference = FALSE,
		KreeFanatic = FALSE,
		ExperimentalWeapons = FALSE,
		HydraAssault = FALSE,
		HydraPatrol = FALSE,
		WeaponMaster = FALSE,
		Temporal = FALSE,
		MasterOfTime = FALSE,
		Anachronauts = FALSE,
		BandOfBadoon = FALSE,
		MenagerieMedley = FALSE,
		SpacePirates = FALSE,
		KreeMilitants = FALSE,
		BadoonHeadhunter = FALSE,
		Heroic = 1,
		CampaignAbsorbingMan = FALSE, CampaignTaskmaster = FALSE, CampaignZola = FALSE, CampaignRedSkull = FALSE,
		CampaignBrotherhood = FALSE, CampaignInfiltrateMuseum = FALSE, CampaignEscapeMuseum = FALSE, CampaignNebula = FALSE, CampaignRonan = FALSE,
		Aggression = FALSE, Justice = FALSE, Leadership = FALSE, Protection = FALSE,
		AntMan = FALSE, BlackPanther = FALSE, BlackWidow = FALSE, CaptainAmerica = FALSE, CaptainMarvel = FALSE,
		DoctorStrange = FALSE, Drax = FALSE, Gamora = FALSE, Groot = FALSE, Hawkeye = FALSE, Hulk = FALSE, IronMan = FALSE, 
		MsMarvel = FALSE, Quicksilver = FALSE,
		RocketRaccoon = FALSE, ScarletWitch = FALSE, SheHulk = FALSE, SpiderMan = FALSE, SpiderWoman = FALSE, StarLord = FALSE, 
		Thor = FALSE, Wasp = FALSE, Venom = FALSE, 
		AntManSolo = FALSE, BlackPantherSolo = FALSE, BlackWidowSolo = FALSE, CaptainAmericaSolo = FALSE, CaptainMarvelSolo = FALSE,
		DoctorStrangeSolo = FALSE, DraxSolo = FALSE, GamoraSolo = FALSE, GrootSolo = FALSE, HawkeyeSolo = FALSE, HulkSolo = FALSE, IronManSolo = FALSE, 
		MsMarvelSolo = FALSE, QuicksilverSolo = FALSE,
		RocketRaccoonSolo = FALSE, ScarletWitchSolo = FALSE, SheHulkSolo = FALSE, SpiderManSolo = FALSE, SpiderWomanSolo = FALSE, StarLordSolo = FALSE,
		ThorSolo = FALSE, WaspSolo = FALSE, VenomSolo = FALSE
		)
	predRange <- predict.glm(marvelGlm, newdata=hardest) - predict.glm(marvelGlm, newdata=easiest)
	# 1.4 is about 80% chance of losing, -2.5 is about 7% chance of losing
	predRange <- 1.4 - -2.5
	# logits <- seq(predict.glm(marvelGlm, newdata=easiest), predict.glm(marvelGlm, newdata=hardest), predRange / 10)
	logits <- seq(-2.5, 1.4, predRange / 10)
	midpoints <- (logits[1:10] + logits[2:11]) / 2
	logitMinusEasiest <- logits - (marvelGlm$coef[1] + coefEasiestScenario)
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

# round((marvelGlm$coef)*10)