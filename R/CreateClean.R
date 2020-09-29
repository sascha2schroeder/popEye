
CreateClean <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: write retrieval functions
  
  cleantmp <- data.frame(matrix(NA, length(dat$item), 5))
  colnames(cleantmp) <- c("subid", "trialid", "trialnum", "itemid", "cond")
  
  # extract values
  cleantmp$trialid <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "trialid"))
  cleantmp$trialnum <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "trialnum"))
  cleantmp$itemid <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "itemid"))
  cleantmp$cond <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "condition"))
  
  cleantmp$calibration.method <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "calibration.method"))
  
  if (is.null(unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "calibration.avg"))) == F) {
    
    cleantmp$calibration.avg <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "calibration.avg"))
    cleantmp$calibration.max <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "calibration.max"))
    
    if (is.na(unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "drift"))) == F) {
      
      cleantmp$drift <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "drift"))
      cleantmp$drift.x <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "drift.x"))
      cleantmp$drift.y <- unlist(lapply(lapply(dat$item, "[[", "meta"), "[[", "drift.y"))
      
    } else {
      
      cleantmp$drift <- NA
      cleantmp$drift.x <- NA
      cleantmp$drift.y <- NA
      
    }
    
  } else {
    
    cleantmp$calibration.avg <- NA
    cleantmp$calibration.max <- NA
    cleantmp$drift <- NA
    cleantmp$drift.x <- NA
    cleantmp$drift.y <- NA
    
  }
  
  cleantmp$trial.calibration <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "trial"), "[[", "calibration"))
  cleantmp$trial.fix <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "trial"), "[[", "nfix"))
  cleantmp$trial.blink <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "trial"), "[[", "blink"))
  cleantmp$trial.crit <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "trial"), "[[", "crit"))
  
  if (env$exp$setup$type == "target"  | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    cleantmp$target.blink <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "blink"))
    cleantmp$target.out <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "out"))
    cleantmp$target.first <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "first"))
    cleantmp$target.pre.blink <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "pre.blink"))
    cleantmp$target.pre.out <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "pre.out"))
    cleantmp$target.pre.launch <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "pre.launch"))
    cleantmp$target.pre.refix <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "pre.refix"))
    cleantmp$target.pre.reg <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "pre.reg"))
    cleantmp$target.post.fix <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "post.fix"))
    cleantmp$target.post.reg <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "post.reg"))
    cleantmp$target.crit <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "target"), "[[", "crit"))
  }
  
  if (env$exp$setup$type == "boundary") {
    cleantmp$boundary.trigger <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "trigger"))
    cleantmp$boundary.seq <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "seq"))
    cleantmp$boundary.blink <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "blink"))
    cleantmp$boundary.out <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "out"))
    cleantmp$boundary.time <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "time"))
    cleantmp$boundary.hook <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "hook"))
    cleantmp$boundary.change.sac <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "change.sac"))
    cleantmp$boundary.change.sac[cleantmp$boundary.change.sac == -999] <- NA
    cleantmp$boundary.pre.time <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "pre.time"))
    cleantmp$boundary.pre.time[cleantmp$boundary.pre.time == -999] <- NA
    cleantmp$boundary.target.time <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "target.time"))
    cleantmp$boundary.target.time[cleantmp$boundary.target.time == -999] <- NA
    cleantmp$boundary.post.time <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "post.time"))
    cleantmp$boundary.post.time[cleantmp$boundary.post.time == -999] <- NA
    cleantmp$boundary.target.fix <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "target.fix"))
    cleantmp$boundary.target.fix[cleantmp$boundary.target.fix == -999] <- NA
    cleantmp$boundary.crit <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "boundary"), "[[", "crit"))
  }
  
  if (env$exp$setup$type == "fast") {
    cleantmp$fast.trigger <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "trigger"))
    cleantmp$fast.blink <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "blink"))
    cleantmp$fast.out <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "out"))
    cleantmp$fast.seq <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "seq"))
    cleantmp$fast.time <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "time"))
    cleantmp$fast.hook <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "hook"))
    
    cleantmp$fast.sac.dur <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "change.sac"))
    cleantmp$fast.sac.dur[cleantmp$fast.sac.dur == -999] <- NA
    
    cleantmp$fast.pre.time <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "pre.time"))
    cleantmp$fast.pre.time[cleantmp$fast.pre.time == -999] <- NA
    
    cleantmp$fast.pre.prime <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "pre.prime"))
    cleantmp$fast.pre.prime[cleantmp$fast.pre.prime == -999] <- NA
    
    cleantmp$fast.prime.time <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "prime.time"))
    cleantmp$fast.prime.time[cleantmp$fast.prime.time == -999] <- NA
    
    cleantmp$fast.post.prime <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "post.prime"))
    cleantmp$fast.post.prime[cleantmp$fast.post.prime == -999] <- NA
    
    cleantmp$fast.fix.dur <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "fix.dur"))
    cleantmp$fast.fix.dur[cleantmp$fast.fix.dur == -999] <- NA
    
    cleantmp$fast.fix.target <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "fix.target"))
    cleantmp$fast.fix.target[cleantmp$fast.fix.target == -999] <- NA
    
    
    cleantmp$fast.crit <- unlist(lapply(lapply(lapply(dat$item, "[[", "clean"), "[[", "fast"), "[[", "crit"))
  }
  
  cleantmp$crit <- unlist(lapply(lapply(dat$item, "[[", "clean"),"[[","crit"))
  
  return(cleantmp)  
  
}
