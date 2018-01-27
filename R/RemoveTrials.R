
RemoveTrials <- function(dat, env = parent.frame(n = 2)){
  
  if (env$exp$setup$tracker$software == "EB") {

    # remove practice trials
    dat$msg <- dat$msg[dat$msg$trialnum > env$exp$setup$clean$practrial, ]
    
    dat$msg$trialnum <- dat$msg$trialnum - env$exp$setup$clean$practrial
    dat$samp <- dat$samp[dat$samp$time > (dat$msg$time[1] - 1), ]
    dat$event <- dat$event[dat$event$time > (dat$msg$time[1] - 1), ]
    
  } else if (env$exp$setup$tracker$software == "ET") {
    
    # remove practice trials
    dat$msg <- dat$msg[dat$msg$trialnum > env$exp$setup$clean$practrial, ]
 
    # extract main trials
    dat$msg <- dat$msg[-grep("99|90", dat$msg$itemid), ]
    
    # TODO: provide item codes to be excluded as argument
   
    dat$msg$trialnum <- as.numeric(factor(dat$msg$trialnum))
    dat$samp <- dat$samp[dat$samp$time > (dat$msg$time[1] - 1), ]
    dat$event <- dat$event[dat$event$time > (dat$msg$time[1] - 1), ]
    
  }
  
  return(dat)
  
}
