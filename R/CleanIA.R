
CleanIA <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: common output format even if no cleaning
  
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    if (env$exp$setup$clean$stage3 == T) {
      dat <- CleanStage3(dat, trial)
      # TODO: no merge step implemented at present
    }
    
    if (env$exp$setup$clean$stage4 == T) {
      dat <- CleanStage4(dat, trial)    
    }
    
    if (env$exp$setup$clean$outlier == T) {
      dat <- CleanOutlier(dat, trial)  
    }
    
    if (env$exp$setup$clean$delete == T) {
      dat <- DeleteFixations(dat, trial)  
    }
    
  }
  
  return(dat)
  
}
