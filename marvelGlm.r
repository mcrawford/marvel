MarvelGlm <- function(marvel) {
  return(glm(formula = (Win == "No") ~
    Scenario * OneHero + DifficultyLevel +
      Undeclared + Timestamp +
      BombScare + MastersOfEvil + UnderAttack + LegionsOfHydra + DoomsdayChair +
      GoblinGimmicks + MessOfThings + PowerDrain + RunningInterference + KreeFanatic +
      ExperimentalWeapons + HydraAssault + HydraPatrol + WeaponMaster +
      Temporal + MasterOfTime + Anachronauts +
      BandOfBadoon + MenagerieMedley + GalacticArtifacts + SpacePirates + KreeMilitants +
      ShipCommand + BadoonHeadhunter +
      BlackOrder + ArmiesOfTitan + ChildrenOfThanos + LegionsOfHel + FrostGiants + Enchantress + InfinityGauntlet +
      BeastyBoys + BrothersGrimm + CrossfiresCrew + MisterHyde + RansackedArmory +
      SinisterSyndicate + StateOfEmergency + StreetsOfMayhem + WreckingCrew +
      CityInChaos + DownToEarth + SymbioticStrength + PersonalNightmare + WhispersOfParanoia +
      GuerrillaTactics + SinisterAssault + GoblinGear + OsbornTech +
      Armadillo + Zzzax + Inheritors + ISSinisterSix + Deathstrike + ShadowKing +
      Acolytes + Brotherhood + FuturePast + Mystique + Sentinels + ZeroTolerance +
      Crime + Fantasy + Horror + SciFi + Sitcom + Western + Longshot +
      BlackTomCassidy + Exodus + ExtremeMeasures + Flight + MilitaryGrade + MutantInsurrection +
      MutantSlayers + NastyBoys + Reavers + Dreadpool + SuperStrength + Telepathy + HopeSummers +
      Infinites + DystopianNightmare + Hounds + DarkRiders + SavageLand +
      Genosha + BlueMoon + CelestialTech + ClanAkkaba + AgeOfApocalypse +
      Sauron + Arcade + CrazyGang + Hellfire +
      AIMAbduction + AIMScience + BatrocsBrigade + ScientistSupreme + GravitationalPull + HardSound +
      PaleLittleSpider + PowerOfTheAtom + Supersonic + TheLeaper + SHIELD +
      Heroic + SkirmishLevel + Standard2 + Expert2 + Standard3 +
      CampaignAbsorbingMan + CampaignTaskmaster + CampaignZola + CampaignRedSkull +
      CampaignBrotherhood + CampaignInfiltrateMuseum + CampaignEscapeMuseum + CampaignNebula + CampaignRonan +
      CampaignEbonyMaw + CampaignTowerDefense + CampaignThanos + CampaignHela + CampaignLoki +
      CampaignSandman + CampaignVenom + CampaignMysterio + CampaignSinisterSix + CampaignVenomGoblin +
      CampaignSabretooth + CampaignProjectWideawake + CampaignMasterMold + CampaignMansionAttack + CampaignMagneto +
      CampaignMorlockSiege + CampaignOnTheRun + CampaignJuggernaut + CampaignMisterSinister + CampaignStryfe +
      CampaignUnus + CampaignFourHorsemen + CampaignApocalypse + CampaignDarkBeast + CampaignEnSabahNur +
      CampaignBlackWidow + CampaignBatroc + CampaignMODOK + CampaignThunderbolts + CampaignBaronZemo +
      Aggression + Justice + Leadership + Protection + Pool +
      Angel + AdamWarlock + AntMan + Bishop + BlackPanther +
      BlackWidow + Cable + CaptainAmerica + CaptainMarvel + Colossus + Cyclops +
      Deadpool + DoctorStrange + Domino + Drax + Gambit + Gamora + GhostSpider + Groot +
      Hawkeye + Hulk + Iceman + IronMan + Ironheart + Magik + Magneto + MariaHill +
      MsMarvel + Nebula + NickFury + Nightcrawler + Nova + Phoenix + Psylocke + Quicksilver + Rogue +
      RocketRaccoon + ScarletWitch + Shadowcat + SheHulk + Spdr + Spectrum +
      SpiderHam + SpiderMan + SpiderManMilesMorales + SpiderWoman + StarLord + Storm +
      Thor + Valkyrie + Venom + Vision + WarMachine + Wasp + Wolverine + X23 +
      AngelSolo + AdamWarlockSolo + AntManSolo + BishopSolo + BlackPantherSolo +
      BlackWidowSolo + CableSolo + CaptainAmericaSolo + CaptainMarvelSolo + ColossusSolo + CyclopsSolo +
      DeadpoolSolo + DoctorStrangeSolo + DominoSolo + DraxSolo + GambitSolo + GamoraSolo + GhostSpiderSolo + GrootSolo +
      HawkeyeSolo + HulkSolo + IcemanSolo + IronManSolo + IronheartSolo + MagikSolo + MagnetoSolo + MariaHillSolo +
      MsMarvelSolo + NebulaSolo + NickFurySolo + NightcrawlerSolo + NovaSolo + PhoenixSolo + PsylockeSolo + QuicksilverSolo + RogueSolo +
      RocketRaccoonSolo + ScarletWitchSolo + ShadowcatSolo + SheHulkSolo + SpdrSolo + SpectrumSolo +
      SpiderHamSolo + SpiderManSolo + SpiderManMilesMoralesSolo + SpiderWomanSolo + StarLordSolo + StormSolo +
      ThorSolo + ValkyrieSolo + VenomSolo + VisionSolo + WarMachineSolo + WaspSolo + WolverineSolo + X23Solo +
      I(InfinityGauntlet == TRUE & DifficultyLevel == "Expert" & Timestamp > "2024-11-01" & Timestamp <= "2024-11-30")
    , family = binomial, data = marvel)
  )
}
