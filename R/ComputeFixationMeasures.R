
ComputeFixationMeasures <- function(dat, env = parent.frame(n = 1)) {
  
  for (trial in 1:length(dat$item)) {
    # trial <- 2
    
    dat <- OutlierAsBlinks(dat, trial)
    # NOTE: not sure whether this function makes sense
    dat <- ComputeLineChange(dat, trial)
    dat <- ComputeSaccadeLength(dat, trial) 
    dat <- ComputeLaunchDistance(dat, trial)
    dat <- ComputeRefixation(dat, trial)
    dat <- ComputeRegression(dat, trial)
    dat <- ComputeFirstskip(dat, trial)
    dat <- ComputeRun(dat, trial)
    dat <- ComputeLandingPosition(dat, trial)
    
    # rename fixid
    dat$item[[trial]]$fix$fixid <- dat$item[[trial]]$fix$num
    dat$item[[trial]]$fix$num <- NULL
    
    if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
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
                 "target",
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
                 "word.firstskip",
                 "word.refix",
                 "word.launch",
                 "word.land", 
                 "word.cland",
                 "word.reg.out",
                 "word.reg.in",
                 "word.reg.out.to",
                 "word.reg.in.from",
                 
                 "ia.fix", 
                 "ia.run", 
                 "ia.runid",
                 "ia.run.fix",
                 "ia.firstskip",
                 "ia.refix",
                 "ia.launch",
                 "ia.land", 
                 "ia.cland",
                 "ia.reg.out", 
                 "ia.reg.in", 
                 "ia.reg.out.to", 
                 "ia.reg.in.from", 
                 
                 "sent.fix",
                 "sent.run",
                 "sent.runid",
                 "sent.run.fix",
                 "sent.firstskip",
                 "sent.refix",
                 "sent.reg.out", 
                 "sent.reg.in",
                 "sent.reg.out.to", 
                 "sent.reg.in.from"
      )
      
    } else {
      
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
                 "line.let",
                 "line.word",
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
                 
                 "word.fix",
                 "word.run",
                 "word.runid",
                 "word.run.fix",
                 "word.firstskip",
                 "word.refix",
                 "word.launch",
                 "word.land", 
                 "word.cland",
                 "word.reg.out",
                 "word.reg.in",
                 "word.reg.out.to",
                 "word.reg.in.from",
                 
                 "ia.fix", 
                 "ia.run", 
                 "ia.runid",
                 "ia.run.fix",
                 "ia.firstskip",
                 "ia.refix",
                 "ia.launch",
                 "ia.land", 
                 "ia.cland",
                 "ia.reg.out", 
                 "ia.reg.in", 
                 "ia.reg.out.to", 
                 "ia.reg.in.from", 
                 
                 "sent.word",
                 "sent.fix",
                 "sent.run",
                 "sent.runid",
                 "sent.run.fix",
                 "sent.firstskip",
                 "sent.refix",
                 "sent.reg.out", 
                 "sent.reg.in",
                 "sent.reg.out.to", 
                 "sent.reg.in.from"
      )
    }
    
    dat$item[[trial]]$fix <- dat$item[[trial]]$fix[names]
    
  }
  
  return(dat)
  
}
