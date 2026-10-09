MarvelGlm <- function(marvel) {
  # Stage 2 reference schemes: Negative Zone Prison (registration) and
  # Secret Avengers (resistance). Each faction chooses one scheme per stage,
  # so including every scheme would make the stage totals linearly dependent.
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
      PaleLittleSpider + PowerOfTheAtom + Supersonic + TheLeaper + SHIELD + TricksterMagic +
      # Additional encounter sets.
      EncounterAllVersusAll + EncounterAtlanteans + EncounterCapeKiller + EncounterCloakDagger +
      EncounterCops + EncounterDangerousRecruits + EncounterDeadlyDuo + EncounterDefenders +
      EncounterDisasters + EncounterDrive + EncounterExtremeRisk + EncounterGrowingStrong +
      EncounterHellsKitchen + EncounterHeroesforHire + EncounterMariaHill + EncounterMartialLaw +
      EncounterMightyAvengers + EncounterMoonKnight + EncounterNamor + EncounterNewAvengers +
      EncounterPaladin + EncounterRoyalGuard + EncounterSHIELDExecutiveBoard + EncounterSHIELDOps +
      EncounterScarletTwins + EncounterSecretAvengers + EncounterSpiderMan + EncounterTaskmaster +
      EncounterTechno + EncounterTheInitiative + EncounterTheOwl + EncounterThunderbolts +
      EncounterTombstone + EncounterTracksuitMafia + EncounterWhiteout + EncounterYoungAvengers +
      Heroic + SkirmishLevel + Standard2 + Expert2 + Standard3 +
      CampaignAbsorbingMan + CampaignTaskmaster + CampaignZola + CampaignRedSkull +
      CampaignBrotherhood + CampaignInfiltrateMuseum + CampaignEscapeMuseum + CampaignNebula + CampaignRonan +
      CampaignEbonyMaw + CampaignTowerDefense + CampaignThanos + CampaignHela + CampaignLoki +
      CampaignSandman + CampaignVenom + CampaignMysterio + CampaignSinisterSix + CampaignVenomGoblin +
      CampaignSabretooth + CampaignProjectWideawake + CampaignMasterMold + CampaignMansionAttack + CampaignMagneto +
      CampaignMorlockSiege + CampaignOnTheRun + CampaignJuggernaut + CampaignMisterSinister + CampaignStryfe +
      CampaignUnus + CampaignFourHorsemen + CampaignApocalypse + CampaignDarkBeast + CampaignEnSabahNur +
      CampaignBlackWidow + CampaignBatroc + CampaignMODOK + CampaignThunderbolts + CampaignBaronZemo +
      CampaignArtMuseumHeist + CampaignTheGetaway + CampaignProtectionRacket + CampaignTheRaftBreakout +
      CampaignStopthePresses + CampaignKingpin +
      # Fear No Evil selected villains.
      VillainBullseye + VillainElectro + VillainHammerhead + VillainPurpleMan + VillainTyphoidMary +
      # Civil War main schemes, grouped by stage.
      Scheme1CutOffSupport + Scheme1EnforcetheLaw + Scheme1GatheringSupport + Scheme1GoingUnderground +
      Scheme1HomelandSecurity + Scheme1OpenRebellion + Scheme1ProtectSecretIdentities +
      Scheme1PublicOutrage + Scheme1RallyingCall + Scheme1SHIELDRecruits +
      Scheme2ExposeOverreach + Scheme2GuerillaWarfare + Scheme2HuntingRebelHeroes +
      Scheme2MightyAvengers + Scheme2NeighborhoodProtectors + Scheme2NoGoingBack +
      Scheme2SuperheroJailbreak + Scheme2TheInitiative +
      Aggression + Justice + Leadership + Protection + Pool +
      Angel + AdamWarlock + AntMan + Bishop + BlackPanther + BlackPantherShuri + BlackWidow + Cable +
      CaptainAmerica + CaptainMarvel + Colossus + Cyclops + Daredevil + Deadpool + DoctorStrange +
      Domino + Drax + Echo + Falcon + Gambit + Gamora + GhostSpider + Groot + Hawkeye + Hercules + Hulk +
      Hulkling + Iceman + IronMan + Ironheart + JessicaJones + Jubilee + LukeCage + Magik + Magneto +
      MariaHill + MsMarvel + Nebula + NickFury + Nightcrawler + Nova + Phoenix + Psylocke + Quicksilver +
      Rogue + RocketRaccoon + ScarletWitch + Shadowcat + SheHulk + Silk + Spdr + Spectrum + SpiderHam +
      SpiderMan + SpiderManMilesMorales + SpiderWoman + StarLord + Storm + Thor + Tigra + Valkyrie +
      Venom + Vision + WarMachine + Wasp + WinterSoldier + Wolverine + WonderMan + X23 +
      AngelSolo + AdamWarlockSolo + AntManSolo + BishopSolo + BlackPantherSolo + BlackPantherShuriSolo +
      BlackWidowSolo + CableSolo + CaptainAmericaSolo + CaptainMarvelSolo + ColossusSolo + CyclopsSolo +
      DaredevilSolo + DeadpoolSolo + DoctorStrangeSolo + DominoSolo + DraxSolo + EchoSolo + FalconSolo +
      GambitSolo + GamoraSolo + GhostSpiderSolo + GrootSolo + HawkeyeSolo + HerculesSolo + HulkSolo +
      HulklingSolo + IcemanSolo + IronManSolo + IronheartSolo + JessicaJonesSolo + JubileeSolo +
      LukeCageSolo + MagikSolo + MagnetoSolo + MariaHillSolo + MsMarvelSolo + NebulaSolo + NickFurySolo +
      NightcrawlerSolo + NovaSolo + PhoenixSolo + PsylockeSolo + QuicksilverSolo + RogueSolo +
      RocketRaccoonSolo + ScarletWitchSolo + ShadowcatSolo + SheHulkSolo + SilkSolo + SpdrSolo +
      SpectrumSolo + SpiderHamSolo + SpiderManSolo + SpiderManMilesMoralesSolo + SpiderWomanSolo +
      StarLordSolo + StormSolo + ThorSolo + TigraSolo + ValkyrieSolo + VenomSolo + VisionSolo +
      WarMachineSolo + WaspSolo + WinterSoldierSolo + WolverineSolo + WonderManSolo + X23Solo +
      I(InfinityGauntlet == TRUE & DifficultyLevel == "Expert" & Timestamp > "2024-11-01" & Timestamp <= "2024-11-30")
    , family = binomial, data = marvel)
  )
}
