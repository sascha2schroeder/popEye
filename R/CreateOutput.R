
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
  env$results <- NULL
  
  # clean
  if (env$exp$setup$type == "text") {
    
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
                             "target.pre.blink",
                             "target.pre.out",
                             "target.pre.launch", 
                             "target.pre.refix",
                             "target.pre.reg", 
                             "target.post.fix",
                             "target.post.reg", 
                             "target.crit", 
                             "crit")
    
  }
  
  if (env$exp$setup$type == "boundary") {
    
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
                             "target.pre.blink",
                             "target.pre.out",
                             "target.pre.launch", 
                             "target.pre.refix",
                             "target.pre.reg", 
                             "target.post.fix",
                             "target.post.reg", 
                             "target.crit",
                             "boundary.trigger", 
                             "boundary.seq", 
                             "boundary.blink",
                             "boundary.out",
                             "boundary.time",
                             "boundary.hook", 
                             "boundary.change.sac", 
                             "boundary.pre.time", 
                             "boundary.target.time", 
                             "boundary.post.time", 
                             "boundary.target.fix", 
                             "boundary.crit", 
                             "crit")
    
  }
  
  if (env$exp$setup$type == "fast") {
    
    env$clean <- data.frame(matrix(NA, 1, 41))
    
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
                             "target.pre.blink",
                             "target.pre.out",
                             "target.pre.launch", 
                             "target.pre.refix",
                             "target.pre.reg", 
                             "target.post.fix",
                             "target.post.reg", 
                             "target.crit",
                             "fast.trigger", 
                             "fast.blink",
                             "fast.out",
                             "fast.seq",
                             "fast.time",
                             "fast.hook", 
                             "fast.change.sac", 
                             "fast.pre.time", 
                             "fast.pre.prime", 
                             "fast.prime.time", 
                             "fast.post.prime", 
                             "fast.fix.dur", 
                             "fast.fix.target", 
                             "fast.crit", 
                             "crit")
    
  }
  
}
