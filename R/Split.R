
# based on Carr et al. (2021)

Split <- function(fixation_XY, line_Y) {
  n <- nrow(fixation_XY)
  diff_X <- diff(fixation_XY[, 1])
  clusters <- kmeans(diff_X, 2)
  sweep_marker <- which.min(clusters$centers)
  end_line_indices <- which(clusters$cluster == sweep_marker)
  end_line_indices <- append(end_line_indices, n)
  start_of_line <- 1
  for (end_of_line in end_line_indices) {
    mean_y <- mean(fixation_XY[start_of_line:end_of_line, 2])
    line_i <- which.min(abs(line_Y - mean_y))
    fixation_XY[start_of_line:end_of_line, 2] <- line_Y[line_i]
    start_of_line <- end_of_line + 1
  }
  return(fixation_XY)
}

