
OutlierAsBlinks <- function(dat, trial) {
  
  # trial = 1
  
  # TODO: check blink definition (1 if blink before or after)
  # NOTE: seperate function?
  # NOTE: remove outliers?
  # NOTE: in previous function (AlignStim)?
  
  # outliers as blinks
  for (j in 2:(nrow(dat$trial[[trial]]$fix) - 1)) {
    if (dat$trial[[trial]]$fix$line[j] == 0) {
      dat$trial[[trial]]$fix$blink[j - 1] <- 1
      dat$trial[[trial]]$fix$blink[j] <- 1
      dat$trial[[trial]]$fix$blink[j + 1] <- 1
    }
  }
  
  return(dat)
  
}