
CleanTrial <- function(dat, env = parent.frame(n = 2)) {
  
  for (trial in 1:length(dat$trial)) {
  # for (trial in 69:69) {
    # trial = 48
    # print(trial)
    
    # set up output slot
    dat$trial[[trial]]$clean$trial <- list(nfix = 0, blink = 0, sac = 0, crit = 0)
    
    # number of fixations
    if (max(dat$trial[[trial]]$fix$fixid) <  env$exp$setup$exclude$nfix) {
      dat$trial[[trial]]$clean$trial$nfix <- 1
    }
    # NOTE: maybe integrate this screening in earlier steps (after outliers are excluded) 
    
    # blink in trial
    if (sum(dat$trial[[trial]]$fix$blink == 1) > 0) {
      dat$trial[[trial]]$clean$trial$blink <- 1
    } 
    
    # screen for long saccades
    if (max(dat$trial[[trial]]$sac$dur[dat$trial[[trial]]$sac$msg == "SAC"]) > env$exp$setup$exclude$sac) {
      dat$trial[[trial]]$clean$trial$sac <- 1
    } 
    # NOTE: maybe integrate this screening in earlier steps  (where saccades are computed)
    
    # combine
    if (env$exp$setup$exclude$blink == T) {
      if (sum(c(dat$trial[[trial]]$clean$trial$nfix, 
                dat$trial[[trial]]$clean$trial$blink,
                dat$trial[[trial]]$clean$trial$sac)) > 0) {
        dat$trial[[trial]]$clean$trial$crit = 1
      }
     } else {
      if (sum(c(dat$trial[[trial]]$clean$trial$nfix, 
                dat$trial[[trial]]$clean$trial$sac)) > 0) {
        dat$trial[[trial]]$clean$trial$crit = 1
      }
    }
    
  }
  
  return(dat)
  
}
