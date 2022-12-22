library(factoextra)

LoadSnap <- function() {
  snapdecks <- read.csv("~/Projects/Personal/marvel/snapdecks.csv")
  snapdecks <- snapdecks[, apply(snapdecks, 2, function(x) { sum(!is.na(x)) > 0 })]
}

PlotClusters <- function(snapdecks) {
  fviz_nbclust(snapdecks[,c(-1,-2)], hcut, method='wss')
  fviz_nbclust(snapdecks[,c(-1,-2)], hcut, method='silhouette')
}

PreviewDecks <- function(snapdecks, n) {
  temp <- hcut(snapdecks[,c(-1,-2)], n)
  for (i in 1:n) {
    print(sort(table(strsplit(paste(snapdecks[temp$cluster==i,2], collapse=" "), "[ ,.\\-\\(\\)\"]"),
                     exclude = c("Pool", "1", "2", "3", "-", "")), decreasing = T)[1:3])
    print(colMeans(snapdecks[temp$cluster == i,c(-1,-2)])[order(colMeans(snapdecks[temp$cluster == i,c(-1,-2)]), decreasing = TRUE)][1:6])
  }

  return(temp)
}

DeckDetail <- function(snapdecks, clusters, x) {
  print(sort(table(strsplit(paste(snapdecks[,2][clusters$cluster==x], collapse=" "), "[ ,.\\-\\(\\)\"]"),
                   exclude = c("Pool", "1", "2", "3", "-", "")), decreasing = T)[1:3])
  print(colMeans(snapdecks[temp$cluster == x,c(-1,-2)])[order(colMeans(snapdecks[temp$cluster == x,c(-1,-2)]), decreasing = TRUE)][1:20])
}