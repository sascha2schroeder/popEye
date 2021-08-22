
CleanTrial <- function(dat, env = parent.frame(n = 2)) {
  
  for (trial in 1:length(dat$item)) {
    # trial = 48
    
    # set up output slot
    dat$item[[trial]]$clean$trial <- list(calibration = 0, 
                                           nfix = 0, 
                                           blink = 0, 
                                           sac = 0, 
                                           crit = 0)
    
    # trial.calibration: if trial was not calibrated
    if (is.na(dat$item[[trial]]$meta$calibration.method) == T | 
        dat$item[[trial]]$meta$calibration.method == "") {
      dat$item[[trial]]$clean$trial$calibration <- 1
      dat$item[[trial]]$clean$trial$crit <- 1
      next
    }
    
    # trial.calibration: if no calibration value is available
    if (length(dat$item[[trial]]$meta$calibration.avg) == 0) {
      dat$item[[trial]]$clean$trial$calibration <- 1
      dat$item[[trial]]$clean$trial$crit <- 1
      next
    }
    
    # trial.calibration: if calibration accuracy was too bad 
    if (dat$item[[trial]]$meta$calibration.avg > 1) {
      dat$item[[trial]]$clean$trial$calibration <- 1
      dat$item[[trial]]$clean$trial$crit <- 1
      next
    }
   
    # trial.fix: check minimum number of fixations in trial (controlled by exclude.fix)
    if (max(dat$item[[trial]]$fix$fixid) <  env$exp$setup$exclude$nfix) {
      dat$item[[trial]]$clean$trial$nfix <- 1
      dat$item[[trial]]$clean$trial$crit <- 1
    }
    # NOTE: maybe integrate this screening in earlier steps (after outliers are excluded) 
    
    # trial.blink: check whether there is a blink in a trial
    if (sum(dat$item[[trial]]$fix$blink == 1) > 0) {
      dat$item[[trial]]$clean$trial$blink <- 1
    } 
    
  }
  
  return(dat)
  
}
