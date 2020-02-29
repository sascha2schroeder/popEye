
CleanTrial <- function(dat, env = parent.frame(n = 2)) {
  
  for (trial in 1:length(dat$trial)) {
  # for (trial in 69:69) {
    # trial = 48
    # print(trial)
    
    # set up output slot
    dat$trial[[trial]]$clean$trial <- list(calibration = 0, 
                                           nfix = 0, 
                                           blink = 0, 
                                           sac = 0, 
                                           crit = 0)
    
    # trial.calibration: if trial was not calibrated
    if (dat$trial[[trial]]$meta$calibration.method == "") {
      dat$trial[[trial]]$clean$trial$calibration <- 1
    }
    
    # trial.calibration: if calibration accuracy was too bad 
    if (dat$trial[[trial]]$meta$calibration.avg > 1) {
      dat$trial[[trial]]$clean$trial$calibration <- 1
    }
    
    # trial.fix: check minimum number of fixations in trial (controlled by exclude.fix)
    if (max(dat$trial[[trial]]$fix$fixid) <  env$exp$setup$exclude$nfix) {
      dat$trial[[trial]]$clean$trial$nfix <- 1
    }
    # NOTE: maybe integrate this screening in earlier steps (after outliers are excluded) 
    
    # trial.blink: check whether there is a blink in a trial
    if (sum(dat$trial[[trial]]$fix$blink == 1) > 0) {
      dat$trial[[trial]]$clean$trial$blink <- 1
    } 
    
    # combine
    if (sum(c(dat$trial[[trial]]$clean$trial$calibration, 
              dat$trial[[trial]]$clean$trial$nfix)) > 0) {
        dat$trial[[trial]]$clean$trial$crit = 1
    }
    
  }
  
  return(dat)
  
}
