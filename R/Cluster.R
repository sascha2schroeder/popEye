
# reimplementation from Carr et al. used

Cluster <- function(fixation_XY, line_Y) {
  n <- nrow(fixation_XY)
  m <- length(line_Y)
  fixation_Y <- fixation_XY[, 2]
  clusters <- kmeans(fixation_Y, m, iter.max=300, nstart=100)
  ordered_cluster_indices <- order(clusters$centers)
  for (fixation_i in 1 : n) {
    cluster_i <- clusters$cluster[fixation_i]
    line_i <- which(ordered_cluster_indices == cluster_i)
    fixation_XY[fixation_i, 2] <- line_Y[line_i]
  }
  return(fixation_XY)
}
