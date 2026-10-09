# Run from the repository root: Rscript tests/test-load-marvel.R
source("marvel.r")

current <- as.data.frame(matrix("", nrow = 4, ncol = 49), stringsAsFactors = FALSE)
current[, 1] <- "2026-10-01 12:00:00"
current[, 2] <- c("Daredevil", "Hulkling", "Echo", "Tigra")
current[, 4] <- "No"
current[, 23] <- c("Rhino", "Art Museum Heist", "Captain America", "God of Lies")
current[, 24] <- c("No", "Yes", "No", "No")
current[, 27] <- "Yes"
current[, 28] <- "Standard (Core Set)"
current[, 30:31] <- 0
current[1, 26] <- "S.H.I.E.L.D. Executive Board, S.H.I.E.L.D. Ops"
current[2, 47] <- "Bullseye"
current[3, 41] <- "Gathering Support"
current[3, 42] <- "Secret Avengers"
current[3, 45] <- "Cloak & Dagger, Spider-Man"
current[, 49] <- "Standard"
path <- tempfile(fileext = ".csv")
write.csv(current, path, row.names = FALSE)
loaded <- LoadMarvel(path)
stopifnot(loaded$Daredevil[1], loaded$DaredevilSolo[1],
          loaded$Hulkling[2], loaded$VillainBullseye[2],
          loaded$CampaignArtMuseumHeist[2],
          !loaded$SHIELD[1], loaded$EncounterSHIELDExecutiveBoard[1],
          loaded$EncounterSHIELDOps[1],
          loaded$Scheme1GatheringSupport[3], loaded$Scheme2SecretAvengers[3],
          loaded$EncounterCloakDagger[3], loaded$EncounterSpiderMan[3],
          !loaded$Undeclared[3])

current[3, 40] <- "Yes"
current[4, 35] <- "Yes"
write.csv(current, path, row.names = FALSE)
filtered <- suppressWarnings(LoadMarvel(path))
stopifnot(!any(filtered$Competitive2 == "Yes"),
          !any(filtered$EpicMultiplayer == "Yes"),
          !any(filtered$First %in% c("Echo", "Tigra")))

unlink(path)
cat("Import, exact encounter matching, configurable scenarios and mode filtering passed.\n")
