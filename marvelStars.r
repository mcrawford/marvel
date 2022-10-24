MarvelStars <- function(marvelGlm) {
  stars <- seq(0, 5, by = .5)
  # 1.0 is about 73% chance of losing, -2.2 is about 10% chance of losing
  predRange <- 1.0 - -2.2
  logits <- seq(-2.2, 1.0, predRange / 10)
  logitMinusEasiest <- logits -
    coef(marvelGlm)["(Intercept)"] -
    (as.numeric(Sys.time()) * coef(marvelGlm)["Timestamp"])
  # The BreakPoint means "any value higher than this is the next level of stars"
  # I.e.,
  #    Stars BreakPoint
  # 1    0.0      -4.67 -- anything higher than -4.7 is 0.5 stars
  # 2    0.5      -0.86 -- anything higher than -0.9 is 1 star
  # 3    1.0       2.95 -- anything higher than 2.9 is 1.5 stars
  return(data.frame(
    Stars = stars,
    BreakPoint = round(logitMinusEasiest * 10, 1),
    Low = c(NA, ceiling(logitMinusEasiest[1:10] * 10)),
    High = floor(logitMinusEasiest * 10)
  ))
}