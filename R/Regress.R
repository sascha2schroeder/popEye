
# Adapted from Carr et al. (under review), Behavior Research Methods

Regress <- function(fixation_XY, line_Y, k_bounds=c(-0.1, 0.1), o_bounds=c(-50, 50), s_bounds=c(1, 20)) {
  n <- nrow(fixation_XY)
  m <- length(line_Y)
  
  fit_lines <- function(params, return_line_assignments=FALSE) {
    k <- k_bounds[1] + (k_bounds[2] - k_bounds[1]) * pnorm(params[1])
    o <- o_bounds[1] + (o_bounds[2] - o_bounds[1]) * pnorm(params[2])
    s <- s_bounds[1] + (s_bounds[2] - s_bounds[1]) * pnorm(params[3])
    predicted_Y_from_slope <- fixation_XY[, 1] * k
    line_Y_plus_offset <- line_Y + o
    density <- matrix(nrow=n, ncol=m)
    for (line_i in 1 : m) {
      fit_Y <- predicted_Y_from_slope + line_Y_plus_offset[line_i]
      density[, line_i] <- log(dnorm(fixation_XY[, 2], fit_Y, s))
    }
    if (return_line_assignments)
      return(apply(density, 1, which.max))
    return(-sum(apply(density, 1, max)))
  }
  
  best_fit <- optim(c(0, 0, 0), fit_lines)
  line_assignments <- fit_lines(best_fit$par, TRUE)
  for (fixation_i in 1 : n) {
    line_i <- line_assignments[fixation_i]
    fixation_XY[fixation_i, 2] <- line_Y[line_i]
  }
  
  line <- as.numeric(as.factor(fixation_XY[,2]))
  
  return(line)
  
}
