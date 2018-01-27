
CleanTrial <- function(dat, env = parent.frame(n = 2)) {
  
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    # set up output slot
    dat$trial[[trial]]$clean$trial <- list(nfix = 0, blink = 0, crit = 0)
    
    # number of fixations
    if (nrow(dat$trial[[trial]]$fix) <  env$exp$setup$exclude$nfix) {
      dat$trial[trial]$clean$trial$nfix <- 1
    }

    # blink in trial
    if (sum(dat$trial[[trial]]$fix$blink == 1) > 0) {
      dat$trial[[trial]]$clean$trial$blink <- 1
    } 
    
    # additional criterion: screen for long saccades
    # exclude very long change saccades (> 80) -> ToDo: data script
    # strial6=strial5[strial5$boundary_sac<100,]
    
    
    # combine

    # if(sum(c(dat$trial[[trial]]$clean$trial$nfix, dat$trial[[trial]]$clean$trial$blink)) > 0) {
    #   print("yes")
    #   dat$trial[[trial]]$clean$trial$crit = 1
    # } else {
    #   print("no")
    # }
        
    if (env$exp$setup$exclude$blink == T) {
      if (sum(c(dat$trial[[trial]]$clean$trial$nfix, dat$trial[[trial]]$clean$trial$blink)) > 0) {
        dat$trial[[trial]]$clean$trial$crit = 1
      }
     } else {
      if (dat$trial[[trial]]$clean$trial$nfix == 1 ) {
        dat$trial[[trial]]$clean$trial$crit = 1
      }
    }
    
  }
  
  return(dat)
  
}
