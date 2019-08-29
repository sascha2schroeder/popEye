
CreateClean <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: write retrieval functions
  
  cleantmp <- data.frame(matrix(NA, length(dat$trial), 5))
  colnames(cleantmp) <- c("subid", "trialid", "trialnum", "itemid", "cond")
  
  
  # extract values
  cleantmp$trialid <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "trialid"))
  cleantmp$trialnum <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "trialnum"))
  cleantmp$itemid <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "itemid"))
  cleantmp$cond <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "condition"))
  
  cleantmp$calibration.method <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "calibration.method"))
  cleantmp$calibration.avg <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "calibration.avg"))
  cleantmp$calibration.max <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "calibration.max"))
  
  if (env$exp$setup$analysis$driftX == T | env$exp$setup$analysis$driftY == T) {
    cleantmp$drift <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "drift"))
    cleantmp$drift.x <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "drift.x"))
    cleantmp$drift.y <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "drift.y"))
  }
  
  cleantmp$trial.fix <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "trial"), "[[", "nfix"))
  cleantmp$trial.blink <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "trial"), "[[", "blink"))
  cleantmp$trial.sac <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "trial"), "[[", "sac"))
  cleantmp$trial.crit <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "trial"), "[[", "crit"))
  
  if (env$exp$setup$type == "target"  | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    cleantmp$target.blink <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "blink"))
    cleantmp$target.out <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "out"))
    cleantmp$target.first <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "first"))
    cleantmp$target.pre.sac <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "pre.sac"))
    cleantmp$target.pre.launch <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "pre.launch"))
    cleantmp$target.pre.refix <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "pre.refix"))
    cleantmp$target.pre.reg <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "pre.reg"))
    cleantmp$target.post.fix <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "post.fix"))
    cleantmp$target.post.sac <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "post.sac"))
    cleantmp$target.post.refix <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "post.refix"))
    cleantmp$target.post.reg <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "post.reg"))
    cleantmp$target.crit <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "crit"))
  }
  
  if (env$exp$setup$type == "boundary") {
    cleantmp$boundary.trigger <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "trigger"))
    cleantmp$boundary.seq <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "seq"))
    cleantmp$boundary.change.sac <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "change.sac"))
    cleantmp$boundary.change.sac[cleantmp$boundary.change.sac == -999] <- NA
    cleantmp$boundary.pre.time <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "pre.time"))
    cleantmp$boundary.pre.time[cleantmp$boundary.pre.time == -999] <- NA
    cleantmp$boundary.target.time <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "target.time"))
    cleantmp$boundary.target.time[cleantmp$boundary.target.time == -999] <- NA
    cleantmp$boundary.post.time <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "post.time"))
    cleantmp$boundary.post.time[cleantmp$boundary.post.time == -999] <- NA
    cleantmp$boundary.target.fix <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "target.fix"))
    cleantmp$boundary.target.fix[cleantmp$boundary.target.fix == -999] <- NA
    cleantmp$boundary.blink <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "blink"))
    cleantmp$boundary.pattern <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "pattern"))
    cleantmp$boundary.time <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "time"))
    cleantmp$boundary.hook <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "hook"))
    cleantmp$boundary.crit <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "boundary"), "[[", "crit"))
  }
  
  if (env$exp$setup$type == "fast") {
    cleantmp$fast.trigger <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "trigger"))
    cleantmp$fast.seq <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "seq"))
    
    cleantmp$fast.sac.dur <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "sac.dur"))
    cleantmp$fast.sac.dur[cleantmp$fast.sac.dur == -999] <- NA
    
    cleantmp$fast.pre.time <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "pre.time"))
    cleantmp$fast.pre.time[cleantmp$fast.pre.time == -999] <- NA
    
    cleantmp$fast.prime.time <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "prime.time"))
    cleantmp$fast.prime.time[cleantmp$fast.prime.time == -999] <- NA
    
    cleantmp$fast.post.prime <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "post.prime"))
    cleantmp$fast.post.prime[cleantmp$fast.post.prime == -999] <- NA
    
    cleantmp$fast.fix.dur <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "fix.dur"))
    cleantmp$fast.fix.dur[cleantmp$fast.fix.dur == -999] <- NA
    
    cleantmp$fast.fix.target <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "fix.target"))
    cleantmp$fast.fix.target[cleantmp$fast.fix.target == -999] <- NA
    
    cleantmp$fast.blink <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "blink"))
    cleantmp$fast.pattern <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "pattern"))
    cleantmp$fast.time <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "time"))
    cleantmp$fast.hook <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "hook"))
    cleantmp$fast.crit <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "fast"), "[[", "crit"))
  }
  
  cleantmp$crit <- unlist(lapply(lapply(dat$trial, "[[", "clean"),"[[","crit"))
  
  return(cleantmp)  
  
}
