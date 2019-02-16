
SelectSac <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: compute saccade length
  # TODO: compute saccade length in letters (from fixation file?)
  # TODO: repair peak velocity
  
  # create output slot
  sac <- dat$trial[[1]]$sac[1, ]
  names <- c("num", "xs", "xe", "msg", "dist.px", "dist.let", "peak.vel", "dur")
  sac <- sac[names]
  
  sac$subid <- NA
  sac$trialid <- NA
  sac$trialnum <- NA
  sac$itemid <- NA
  sac$cond <- NA

  # trial loop
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    # temporary fix frame
    sactmp <- dat$trial[[trial]]$sac
    names <- c("num", "xs", "xe", "msg", "dist.px", "dist.let", "peak.vel", "dur")
    sactmp <- sactmp[names]
    
    # if (env$exp$setup$tracker$calibration != "H3") {
    #   names <- c("num", "dx", "dy", "peak.vel", "dur")
    # } else {
    #   names <- c("num", "dx", "peak.vel", "dur")
    # }
    # sactmp <- sactmp[names]
    
    sactmp$subid <- "dummy"
    sactmp$trialid <- dat$trial[[trial]]$meta$trialid
    sactmp$trialnum <- dat$trial[[trial]]$meta$trialnum
    sactmp$itemid <- dat$trial[[trial]]$meta$itemid
    sactmp$cond <- dat$trial[[trial]]$meta$cond
    
    # add to output
    sac <- rbind(sac, sactmp)
    
    # print(trial)

  }
  
  # rename fixid
  sac$sacid <- sac$num
  
  # select variables
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "sacid",
             "msg", "xs", "xe", "dist.px", "dist.let", "peak.vel", "dur")
  sac <- sac[names]
  
  # # select variables
  # if (env$exp$setup$tracker$calibration != "H3") {
  #   names <- c("subid", "trialid", "trialnum", "itemid", "cond", "sacid",
  #              "dx", "dy", "peak.vel", "dur")
  # } else {
  #   names <- c("subid", "trialid", "trialnum", "itemid", "cond", "sacid",
  #              "dx", "peak.vel", "dur")
  # }
  
  # TODO: compute saccade length (or use letters in fixation report?)
  
  return(sac)
  
}
