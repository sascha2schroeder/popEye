
CleanAll <- function(dat, env = parent.frame(n = 1)) {
  
  # trial cleaning
  dat <- CleanTrial(dat)
  
  # target cleaning
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary") {
    dat <- CleanTarget(dat)
  }
  
  # boundary cleaning
  if (env$exp$setup$type == "boundary") {
    dat <- CleanBoundary(dat)
  }
  
  
  # combine
  # --------


  for (trial in 1:length(dat$trial)) {

    dat$trial[[trial]]$clean$crit <- 0

    # sentence
    if (sum(c(dat$trial[[trial]]$clean$trial$crit)) > 0) {
      dat$trial[[trial]]$clean$crit = 1
    }

    # target
    if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary") {
      if (sum(c(dat$trial[[trial]]$clean$trial$crit,
                dat$trial[[trial]]$clean$target$crit)) > 0) {
        dat$trial[[trial]]$clean$crit = 1
      }
    }

    # boundary
    if (env$exp$setup$type == "boundary") {
      if (sum(c(dat$trial[[trial]]$clean$trial$crit,
                dat$trial[[trial]]$clean$target$crit,
                dat$trial[[trial]]$clean$boundary$crit)) > 0) {
        dat$trial[[trial]]$clean$crit = 1
      }
    }

  }
  
  return(dat)
  
}
