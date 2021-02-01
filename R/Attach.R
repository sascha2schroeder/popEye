
Attach <- function(fix, stimmat, env = parent.frame(n = 3)) {
  
  fix$type <- "in"
  fix$linerun <- NA
  fix$run <- NA
  
  for (i in 1:nrow(fix)) {
    # i <- 2
    
    out <- abs(fix$yn[i] - stimmat$ym)
    
    if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$assign$outlierY) {
      fix$type[i] <- "out"
      fix$line[i] <- NA
    } else {
      fix$line[i] <- stimmat$line[which.min(out)]
    }
    
  }
  
  return(fix)
  
}
