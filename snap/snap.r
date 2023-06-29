library(factoextra)

LoadSnap <- function() {
  snapdecks <- read.csv("~/Projects/Personal/marvel/snap/snapdecks.csv")
  snapdecks <- snapdecks[, apply(snapdecks, 2, function(x) { sum(!is.na(x)) > 0 })]
}

PlotClusters <- function(snapdecks) {
  fviz_nbclust(snapdecks[, c(-1, -2, -3)], hcut, method = 'wss')
  fviz_nbclust(snapdecks[, c(-1, -2, -3)], hcut, method = 'silhouette')
}

PreviewDecks <- function(snapdecks, n) {
  temp <- hcut(snapdecks[, c(-1, -2)], n)
  for (i in 1:n) {
    words <- paste(
      as.data.frame(
        sort(
          table(
            strsplit(paste(snapdecks[temp$cluster == i, 1], collapse = " "), "[ ,.\\-\\(\\)\"]"),
            exclude = c("Pool", "1", "2", "3", "4", "5", "-", "", "Deck")), decreasing = T)[1:3]
      )$Var1, collapse = " "
    )
    info <- paste(words, "-", temp$size[i], "decks", "-", round(mean(snapdecks$views[temp$cluster == i]), 0), "views")
    cat(info, "\n\n")
    print(
      colMeans(snapdecks[temp$cluster == i, c(-1, -2)])
      [order(colMeans(snapdecks[temp$cluster == i, c(-1, -2)]), decreasing = TRUE)]
      [1:6]
    )
  }

  return(temp)
}

DeckDetail <- function(snapdecks, clusters, x) {
  words <- paste(
    as.data.frame(
      sort(
        table(
          strsplit(paste(snapdecks[clusters$cluster == x, 1], collapse = " "), "[ ,.\\-\\(\\)\"]"),
          exclude = c("Pool", "1", "2", "3", "4", "5", "-", "", "Deck")), decreasing = T)[1:3]
    )$Var1, collapse = " "
  )
  info <- paste(words, "-", clusters$size[x], "decks", "-", round(mean(snapdecks$views[clusters$cluster == x]), 0), "views")
  cat(info, "\n\n")
  print(colMeans(snapdecks[clusters$cluster == x, c(-1, -2)])[order(colMeans(snapdecks[clusters$cluster == x, c(-1, -2)]), decreasing = TRUE)][1:20])
}