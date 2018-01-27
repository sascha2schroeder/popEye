
ComputeClean <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: write retrieval functions
  
  cleantmp <- data.frame(matrix(NA, length(dat$trial), 3))
  colnames(cleantmp) <- c("subid", "trialnum", "cond")
  
  # extract values
  cleantmp$trialnum <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "trialnum"))
  cleantmp$cond <- unlist(lapply(lapply(dat$trial, "[[", "meta"), "[[", "condition"))
  
  cleantmp$trial.fix <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "trial"), "[[", "nfix"))
  cleantmp$trial.blink <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "trial"), "[[", "blink"))
  cleantmp$trial.crit <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "trial"), "[[", "crit"))
  
  if (env$exp$setup$type == "target"  | env$exp$setup$type == "boundary") {
    cleantmp$target.fix <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "fix"))
    cleantmp$target.blink <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "blink"))
    cleantmp$target.pre.sac <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "pre.sac"))
    cleantmp$target.pre.skip <- unlist(lapply(lapply(lapply(dat$trial, "[[", "clean"), "[[", "target"), "[[", "pre.skip"))
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
  
  cleantmp$crit <- unlist(lapply(lapply(dat$trial, "[[", "clean"),"[[","crit"))
  
  return(cleantmp)  
  
}
