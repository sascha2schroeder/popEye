
ComputeFixationMeasures <- function(dat) {
  
  for (trial in 1:length(dat$trial)) {
    # trial <- 2
    
    # dat <- OutlierAsBlinks(dat, trial) 
    # NOTE: not sure whether this function makes sense
    
    dat <- ComputeLineChange(dat, trial)
    dat <- ComputeSaccadeLength(dat, trial) 
    dat <- ComputeLaunchDistance(dat, trial)
    dat <- ComputeRefixation(dat, trial)
    dat <- ComputeRegression(dat, trial)
    dat <- ComputeSkip(dat, trial)
    dat <- ComputeRun(dat, trial)
    dat <- ComputeLandingPosition(dat, trial)
    
    # rename fixid
    dat$trial[[trial]]$fix$fixid <- dat$trial[[trial]]$fix$num
    dat$trial[[trial]]$fix$num <- NULL
    
    names <- c("subid", 
               "trialid", 
               "trialnum", 
               "itemid", 
               "cond", 
               "fixid", 
               "start",
               "stop", 
               "xs", 
               "ys", 
               "xn", 
               "yn", 
               "ym", 
               "dur", 
               "sac.in",
               "sac.out",
               "type", 
               "blink",            
               "line", 
               "line.change",
               "letternum", 
               "letter", 
               "wordnum", 
               "word", 
               "ianum", 
               "ia",
               "sentnum", 
               "sent", 
               "sent.nwords", 
               "trial", 
               "trial.nwords", 
               "line.let",
               "line.word",
               "sent.word",
               
               "word.fix",
               "word.run",
               "word.runid",
               "word.run.fix",
               "word.skip",
               "word.firstskip",
               "word.refix",
               "word.launch",
               "word.land", 
               "word.cland",
               "word.reg.out",
               "word.reg.in",
               
               "ia.fix", 
               "ia.run", 
               "ia.runid",
               "ia.run.fix",
               "ia.skip",
               "ia.firstskip",
               "ia.refix",
               "ia.launch",
               "ia.land", 
               "ia.cland",
               "ia.reg.out", 
               "ia.reg.in", 
               
               "sent.fix",
               "sent.run",
               "sent.runid",
               "sent.run.fix",
               "sent.skip",
               "sent.firstskip",
               "sent.refix",
               "sent.reg.out", 
               "sent.reg.in"
               )
    
    dat$trial[[trial]]$fix <- dat$trial[[trial]]$fix[names]
    
    # print(trial)
    
  }
  
  
  return(dat)
  
}
