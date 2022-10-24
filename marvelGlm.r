MarvelGlm <- function(marvel) {
  return(glm(formula = (Win == "No") ~
    Scenario * OneHero + DifficultyLevel +
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
      Armadillo + Zzzax +
      Acolytes + Brotherhood + Mystique + Sentinels + ZeroTolerance +
      Heroic + SkirmishLevel + Standard2 + Expert2 +
      CampaignAbsorbingMan + CampaignTaskmaster + CampaignZola + CampaignRedSkull +
      CampaignBrotherhood + CampaignInfiltrateMuseum + CampaignEscapeMuseum + CampaignNebula + CampaignRonan +
      CampaignEbonyMaw + CampaignTowerDefense + CampaignThanos + CampaignHela + CampaignLoki +
      CampaignSandman + CampaignVenom + CampaignMysterio + CampaignSinisterSix + CampaignVenomGoblin +
      CampaignSabretooth + CampaignProjectWideawake + CampaignMasterMold + CampaignMansionAttack + CampaignMagneto +
      Aggression + Justice + Leadership + Protection +
      AdamWarlock + AntMan + BlackPanther +
      BlackWidow + CaptainAmerica + CaptainMarvel + Colossus + Cyclops +
      DoctorStrange + Drax + Gamora + GhostSpider + Groot +
      Hawkeye + Hulk + IronMan + Ironheart +
      MsMarvel + Nebula + Nova + Phoenix + Quicksilver +
      RocketRaccoon + ScarletWitch + Shadowcat + SheHulk + Spdr + Spectrum +
      SpiderHam + SpiderMan + SpiderManMilesMorales + SpiderWoman + StarLord +
      Thor + Valkyrie + Vision + WarMachine + Wasp + Venom +
      AdamWarlockSolo + AntManSolo + BlackPantherSolo +
      BlackWidowSolo + CaptainAmericaSolo + CaptainMarvelSolo +
      DoctorStrangeSolo + DraxSolo + GamoraSolo + GhostSpiderSolo + GrootSolo +
      HawkeyeSolo + HulkSolo + IronManSolo + IronheartSolo + NebulaSolo + NovaSolo +
      MsMarvelSolo + QuicksilverSolo +
      RocketRaccoonSolo + ScarletWitchSolo + SheHulkSolo + SpectrumSolo +
      SpiderManSolo + SpiderManMilesMoralesSolo + SpiderWomanSolo + StarLordSolo +
      ThorSolo + ValkyrieSolo + VisionSolo + WarMachineSolo + WaspSolo + VenomSolo
    , family = binomial, data = marvel)
  )
}