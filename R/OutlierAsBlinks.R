
OutlierAsBlinks <- function(dat, trial) {
  
  # NOTE: Integrate in previous function (AlignStim)?
  
  for (i in 2:(nrow(dat$trial[[trial]]$fix) - 1)) {
    if (is.na(dat$trial[[trial]]$fix$line[i])) {
      dat$trial[[trial]]$fix$blink[i - 1] <- 1
      dat$trial[[trial]]$fix$blink[i] <- 1
      dat$trial[[trial]]$fix$blink[i + 1] <- 1
    }
  }
  
  return(dat)
  
}