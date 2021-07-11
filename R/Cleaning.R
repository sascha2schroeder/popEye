
Cleaning <- function(out, env = parent.frame(n = 2)) {
  
  out <- CleanFixations(out, 
                        dur.thresh = env$exp$setup$clean$stage1Dur, 
                        dist.thresh = env$exp$setup$clean$stage1Dist)
  
  
  out <- CleanFixations(out, 
                        dur.thresh = env$exp$setup$clean$stage2Dur,
                        dist.thres = env$exp$setup$clean$stage2Dist)
  
  return(out)
  
}