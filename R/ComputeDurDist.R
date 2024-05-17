
ComputeDurDist <- function(fix, env = parent.frame(n = 4)){

  # compute fixation duration
  fix$dur <- fix$stop - fix$start
  
  # compute distance to previous fixation (in characters)
  fix$dist = NA
  for (i in 2:nrow(fix)){
      fix$dist[i] <- round(abs(fix$xs[i] - fix$xs[i - 1]) / 
                             env$exp$setup$font$size, 2)  
  }
  
  return(fix)

}
