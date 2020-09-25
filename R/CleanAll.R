
CleanAll <- function(dat, env = parent.frame(n = 1)) {
  
  # trial cleaning
  dat <- CleanTrial(dat)
  
  # target cleaning
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    dat <- CleanTarget(dat)
  }

  # boundary cleaning
  if (env$exp$setup$type == "boundary") {
    dat <- CleanBoundary(dat)
  }

  # fast priming cleaning
  if (env$exp$setup$type == "fast") {
    dat <- CleanFast(dat)
  }

  
  # combine
  # --------

  for (trial in 1:length(dat$item)) {

    dat$item[[trial]]$clean$crit <- 0

    # sentence
    if (sum(c(dat$item[[trial]]$clean$trial$crit)) > 0) {
      dat$item[[trial]]$clean$crit = 1
    }

    # target
    if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
      if (sum(c(dat$item[[trial]]$clean$trial$crit,
                dat$item[[trial]]$clean$target$crit)) > 0) {
        dat$item[[trial]]$clean$crit = 1
      }
    }

    # boundary
    if (env$exp$setup$type == "boundary") {
      if (sum(c(dat$item[[trial]]$clean$trial$crit,
                dat$item[[trial]]$clean$target$crit,
                dat$item[[trial]]$clean$boundary$crit)) > 0) {
        dat$item[[trial]]$clean$crit = 1
      }
    }

    # fast priming
    if (env$exp$setup$type == "fast") {
      if (sum(c(dat$item[[trial]]$clean$trial$crit,
                dat$item[[trial]]$clean$target$crit,
                dat$item[[trial]]$clean$fast$crit)) > 0) {
        dat$item[[trial]]$clean$crit = 1
      }
    }
  }
  
  return(dat)
  
}
