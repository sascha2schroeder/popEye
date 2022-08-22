
# based on Lohmeier (2015), reimplementation based on Carr et al. (2021)

Stretch <- function(fixation_XY, line_Y, scale_bounds=c(0.9, 1.1), offset_bounds=c(-50, 50)) {
  n <- nrow(fixation_XY)
  fixation_Y <- fixation_XY[, 2]
  
  fit_lines <- function(params, return_correction=FALSE) {
    candidate_Y <- fixation_Y * params[1] + params[2]
    corrected_Y <- integer(n)
    for (fixation_i in 1 : n) {
      line_i <- which.min(abs(line_Y - candidate_Y[fixation_i]))
      corrected_Y[fixation_i] <- line_Y[line_i]
    }
    if (return_correction)
      return(corrected_Y)
    return(sum(abs(candidate_Y - corrected_Y)))
  }
  
  lower_bounds <- c(scale_bounds[1], offset_bounds[1])
  upper_bounds <- c(scale_bounds[2], offset_bounds[2])
  best_fit <- optim(c(1, 0), fit_lines, method='L-BFGS-B', lower=lower_bounds, upper=upper_bounds)
  fixation_XY[, 2] <- fit_lines(best_fit$par, return_correction=TRUE)
  return(fixation_XY)
}