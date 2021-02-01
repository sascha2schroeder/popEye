
Chain <- function (fix, stimmat, env = parent.frame(n = 3)) {
  
  # compute distance
  fix$distx <- NA
  fix$distx[2:length(fix$distx)] <- diff(fix$xn)
  fix$disty <- NA
  fix$disty[2:length(fix$disty)] <- diff(fix$yn)
  
  # compute mean y position of lines
  linem <- tapply(stimmat$ym, stimmat$line, mean)
  
  # initialize variables
  fix$type <- "in"
  fix$run <- NA
  fix$linerun <- NA
  fix$linerun[1] <- 1
  fix$line <- NA
  
  mem <- NULL
  start <- 1
  stop <- nrow(linem)
  
  # segment into runs
  for (i in 2:nrow(fix)) {
    
    # determine run break
    if (abs(fix$disty[i]) >= env$exp$setup$font$height * env$exp$setup$assign$lineY | 
        abs(fix$distx[i]) >= env$exp$setup$font$height * env$exp$setup$assign$lineX) {
      
      # assign previous run to line
      mean.y <- mean(fix$yn[fix$linerun == fix$linerun[i - 1]], na.rm = T)
      
      if (mean.y > (max(stimmat$ye) + env$exp$setup$font$height * env$exp$setup$assign$outlierY) | 
          mean.y < (min(stimmat$ys) - env$exp$setup$font$height * env$exp$setup$assign$outlierY)) {
        
        fix$type[fix$linerun == fix$linerun[i - 1]] <- "out"
        
      } else {
        
        out <- NULL
        
        for (j in start:stop) {
          out[j] <- (mean.y - linem[j])^2
        }
        
        fix$line[fix$linerun == fix$linerun[i - 1]] <- which.min(out)
        
      }
      
      fix$linerun[i] <- fix$linerun[i - 1] + 1
      
    } else {
      
      fix$linerun[i] <- fix$linerun[i - 1]
      
    }
    
  }
  
  # assign last run
  mean.y <- mean(fix$yn[fix$linerun == fix$linerun[nrow(fix)]], na.rm = T)
  
  if (mean.y > (max(stimmat$ye) + env$exp$setup$font$height * env$exp$setup$assign$outlierY) | 
      mean.y < (min(stimmat$ys) - env$exp$setup$font$height * env$exp$setup$assign$outlierY)) {
    
    fix$type[fix$linerun == fix$linerun[nrow(fix)]] <- "out"
    
  } else {
    
    out <- NULL
    
    for (j in 1:nrow(linem)) {
      out[j] <- (mean.y - linem[j])^2
    }
    
    fix$line[fix$linerun == fix$linerun[nrow(fix)]] <- which.min(out)
    
  }
  
  return(fix)
  
}