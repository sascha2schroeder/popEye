
# Adapted from Carr et al. (under review), Behavior Research Methods

Warp <- function(fixation_XY, word_XY) {
  n <- nrow(fixation_XY)
  dtw <- dynamic_time_warping(fixation_XY, word_XY)
  for (fixation_i in 1 : n) {
    words_mapped_to_fixation_i <- unlist(dtw$path[[fixation_i]])
    candidate_Y <- word_XY[words_mapped_to_fixation_i, 2]
    fixation_XY[fixation_i, 2] <- mode(candidate_Y)
  }
  return(fixation_XY)
}

mode <- function(values) {
  unique_values <- unique(values)
  return(unique_values[which.max(tabulate(match(values, unique_values)))])
}

dynamic_time_warping <- function(sequence1, sequence2) {
  n1 <- nrow(sequence1)
  n2 <- nrow(sequence2)
  dtw_cost <- matrix(nrow=n1+1, ncol=n2+1)
  dtw_cost[1, ] <- Inf
  dtw_cost[ ,1] <- Inf
  dtw_cost[1,1] <- 0
  for (i in 1 : n1) {
    for (j in 1 : n2) {
      this_cost <- sqrt(sum((sequence1[i,] - sequence2[j,])^2))
      dtw_cost[i+1, j+1] <- this_cost + min(dtw_cost[i, j+1], dtw_cost[i+1, j], dtw_cost[i, j])
    }
  }
  dtw_cost <- dtw_cost[2:(n1+1), 2:(n2+1)]
  dtw_path <- replicate(n1, list())
  while (i > 1 | j > 1) {
    dtw_path[[i]] <- append(dtw_path[[i]], j)
    possible_moves <- c(Inf, Inf, Inf)
    if (i > 1 & j > 1)
      possible_moves[1] <- dtw_cost[i-1, j-1]
    if (i > 1)
      possible_moves[2] <- dtw_cost[i-1, j]
    if (j > 1)
      possible_moves[3] <- dtw_cost[i, j-1]
    best_move <- which.min(possible_moves)
    if (best_move == 1) {
      i <- i - 1
      j <- j - 1
    }
    else if (best_move == 2)
      i <- i - 1
    else
      j <- j - 1
  }
  dtw_path[[1]] <- append(dtw_path[[1]], 1)
  return(list('cost' = dtw_cost[n1, n2], 'path' = dtw_path))
}
