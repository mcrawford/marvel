# Marvel Champions difficulty ratings

Download the **Form Responses 1** tab of the source workbook as CSV. The
Overview tab contains aggregate statistics rather than the game records needed
by the model.

Source: https://docs.google.com/spreadsheets/d/1EConYNxq5QZLtlnWsek1VEr1aiBzIQ0h-_6zNiTT35A/edit

Run from this directory in R:

```r
source("marvel.r")
games <- LoadMarvel("/path/to/form-responses.csv")
fit <- MarvelGlm(games)
MarvelFactors(fit)
MarvelHeroes(fit)
MarvelEncounterSets(fit)
MarvelStars(fit)
```

`LoadMarvel()` assigns column names for the current 49-column export. Competitive and Epic
Multiplayer games are excluded from cooperative ratings, with a warning giving
the excluded count. Encounter labels are matched exactly, including sets in the
two Civil War faction-specific fields.

Fear No Evil uses separate scenario, selected-villain and campaign effects.
Civil War uses leader/scenario effects and additive stage 1 and stage 2 scheme
effects. Negative Zone Prison and Secret Avengers are the registration and
resistance stage 2 reference schemes, respectively; each has a zero adjustment.
Other stage 2 adjustments are relative to their faction's reference. These
references prevent linear dependence between the two stages' scheme totals.

`MarvelEncounterSets()` includes encounter adjustments, selected-villain
adjustments, Civil War scheme adjustments and difficulty modifiers. Combine the
appropriate adjustments with the scenario and hero ratings. A positive rating
increases difficulty; a negative rating decreases it. The existing scenario and
encounter pseudo-game priors and star calibration are retained.

Run the import and feature regression checks with:

```sh
Rscript tests/test-load-marvel.R
```
