
OutlierAsBlinks <- function(dat, trial) {
  
  # NOTE: Integrate in previous function (AlignStim)?
  
  for (i in 2:(nrow(dat$item[[trial]]$fix) - 1)) {
    if (is.na(dat$item[[trial]]$fix$line[i])) {
      dat$item[[trial]]$fix$blink[i - 1] <- 1
      dat$item[[trial]]$fix$blink[i] <- 1
      dat$item[[trial]]$fix$blink[i + 1] <- 1
    }
  }
  
  return(dat)
  
}