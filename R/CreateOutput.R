
CreateOutput <- function(env = parent.frame(n = 1)) {
  
  # item files
  env$word.item <- data.frame(matrix(NA, 1, 8))
  colnames(env$word.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", 
                               "sentnum", "wordnum", "word")
  
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    env$ia.item <- data.frame(matrix(NA, 1, 9))
    colnames(env$ia.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", 
                               "sentnum", "ianum", "ia", "target")
  } else {
    env$ia.item <- data.frame(matrix(NA, 1, 8))
    colnames(env$ia.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", 
                               "sentnum", "ianum", "ia")
  }
  
  env$sent.item <- data.frame(matrix(NA, 1, 7))
  colnames(env$sent.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", 
                               "sentnum", "sent")
  
  # fix
  env$fix <- NULL
  
  # sac
  env$sac <- NULL
  
  # results
  env$results <- list(text = NA, quest = NA)
  
  # clean
  if (env$exp$setup$type == "text") {
    
    # if (env$exp$setup$analysis$driftX == T | env$exp$setup$analysis$driftY == T) {
      
      env$clean <- data.frame(matrix(NA, 1, 16))
      colnames(env$clean) <- c("subid", 
                               "trialid", 
                               "trialnum", 
                               "itemid", 
                               "cond",
                               "calibration.method",
                               "calibration.avg",
                               "calibration.max",
                               "drift",
                               "drift.x",
                               "drift.y",
                               "trial.calibration",
                               "trial.fix", 
                               "trial.blink", 
                               "trial.crit", 
                               "crit")
      
  } 
  
  
  if (env$exp$setup$type == "sentence") {
    
    # if (env$exp$setup$analysis$driftX == T | env$exp$setup$analysis$driftY == T) {
      
    env$clean <- data.frame(matrix(NA, 1, 16))
    colnames(env$clean) <- c("subid", 
                             "trialid", 
                             "trialnum", 
                             "itemid", 
                             "cond",
                             "calibration.method",
                             "calibration.avg",
                             "calibration.max",
                             "drift",
                             "drift.x",
                             "drift.y",
                             "trial.calibration",
                             "trial.fix", 
                             "trial.blink", 
                             "trial.crit", 
                             "crit")
    
  } 
  
  if (env$exp$setup$type == "target") {
    
    # if (env$exp$setup$analysis$driftX == T | env$exp$setup$analysis$driftY == T) {
      
    env$clean <- data.frame(matrix(NA, 1, 27))
    colnames(env$clean) <- c("subid", 
                             "trialid", 
                             "trialnum", 
                             "itemid", 
                             "cond",
                             "calibration.method",
                             "calibration.avg",
                             "calibration.max",
                             "drift",
                             "drift.x",
                             "drift.y",
                             "trial.calibration",
                             "trial.fix", 
                             "trial.blink", 
                             "trial.crit", 
                             "target.blink", 
                             "target.out",
                             "target.first",
                             "target.pre.sac", 
                             "target.pre.launch", 
                             "target.pre.refix",
                             "target.pre.reg", 
                             "target.post.fix",
                             "target.post.sac", 
                             "target.post.reg", 
                             "target.crit", 
                             "crit")
    
  }
  
  if (env$exp$setup$type == "boundary") {
    
    # if (env$exp$setup$analysis$driftX == T | env$exp$setup$analysis$driftY == T) {
      
    env$clean <- data.frame(matrix(NA, 1, 39))
    colnames(env$clean) <- c("subid", 
                             "trialid", 
                             "trialnum", 
                             "itemid", 
                             "cond",
                             "calibration.method",
                             "calibration.avg",
                             "calibration.max",
                             "drift",
                             "drift.x",
                             "drift.y",
                             "trial.calibration",
                             "trial.fix", 
                             "trial.blink", 
                             "trial.crit", 
                             "target.blink",
                             "target.out",
                             "target.first", 
                             "target.pre.sac",
                             "target.pre.launch", 
                             "target.pre.refix",
                             "target.pre.reg", 
                             "target.post.fix",
                             "target.post.sac", 
                             "target.post.reg", 
                             "target.crit",
                             "boundary.trigger", 
                             "boundary.seq", 
                             "boundary.change.sac", 
                             "boundary.pre.time", 
                             "boundary.target.time", 
                             "boundary.post.time", 
                             "boundary.target.fix", 
                             "boundary.blink",
                             "boundary.pattern", 
                             "boundary.time",
                             "boundary.hook", 
                             "boundary.crit", 
                             "crit")
    
  }
  
  # TODO: fast priming outdated
  if (env$exp$setup$type == "fast") {
    
    # if (env$exp$setup$analysis$driftX == T | env$exp$setup$analysis$driftY == T) {
    
    env$clean <- data.frame(matrix(NA, 1, 40))
    
    colnames(env$clean) <- c("subid", 
                             "trialid", 
                             "trialnum", 
                             "itemid", 
                             "cond",
                             "calibration.method",
                             "calibration.avg",
                             "calibration.max",
                             "drift",
                             "drift.x",
                             "drift.y",
                             "trial.calibration",
                             "trial.fix", 
                             "trial.blink", 
                             "trial.crit", 
                             "target.blink",
                             "target.out",
                             "target.first",
                             "target.pre.sac",
                             "target.pre.launch", 
                             "target.pre.refix",
                             "target.pre.reg", 
                             "target.post.fix",
                             "target.post.sac", 
                             "target.post.reg", 
                             "target.crit",
                             "fast.trigger", 
                             "fast.seq", 
                             "fast.sac.dur", 
                             "fast.pre.time", 
                             "fast.prime.time", 
                             "fast.post.prime", 
                             "fast.fix.dur", 
                             "fast.fix.target", 
                             "fast.blink",
                             "fast.pattern", 
                             "fast.time",
                             "fast.hook", 
                             "fast.crit", 
                             "crit")
    
  }
  
}
