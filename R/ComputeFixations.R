
ComputeFixations <- function(xy, sac, env = parent.frame(n = 3)){
  
  if (sac$start[1] > 1 & (sac$stop[nrow(sac)] + 1) < max(xy$time)) {
    # starts with fixation/blink, ends with fixation/blink
    
    fix = data.frame(matrix(NA, nrow(sac) + 1, 9))
    colnames(fix) = c("num", "start", "stop", "xs", "ys", "xe", "ye", 
                      "xdrift", "ydrift")  
    
    # first fixation 
    fix$num[1] <- 1
    fix$start[1] <- 1
    fix$stop[1] <- sac$start[1] - 1
    fix$xs[1] <- round(median(xy$x[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$ys[1] <- round(median(xy$y[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$xdrift[1] <- round(MedianSD(xy$x[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$ydrift[1] <- round(MedianSD(xy$y[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    
    if (nrow(fix) > 2) { # FIX: do only if more than two fixations
      
      # other fixations
      for (i in 2:(nrow(fix) - 1)){
        fix$num[i] <- i
        fix$start[i] <- sac$stop[i - 1] + 1
        fix$stop[i] = sac$start[i] - 1
        fix$xs[i] <- round(median(xy$x[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$ys[i] <- round(median(xy$y[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$xdrift[i] <- round(MedianSD(xy$x[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$ydrift[i] <- round(MedianSD(xy$y[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
      }
      
    }
    
    # last fixation 
    fix$num[nrow(fix)] <- nrow(fix)
    fix$start[nrow(fix)] <-  sac$stop[nrow(sac)] + 1
    fix$stop[nrow(fix)] <-  max(xy$time)
    fix$xs[nrow(fix)] = round(median(xy$x[xy$time >= fix$start[nrow(fix)] & xy$time <= fix$stop[nrow(fix)]]))
    fix$ys[nrow(fix)] = round(median(xy$y[xy$time >= fix$start[nrow(fix)] & xy$time <= fix$stop[nrow(fix)]]))
    fix$xdrift[nrow(fix)] = round(MedianSD(xy$x[xy$time >= fix$start[nrow(fix)] & xy$time <= fix$stop[nrow(fix)]]))
    fix$ydrift[nrow(fix)] = round(MedianSD(xy$y[xy$time >= fix$start[nrow(fix)] & xy$time <= fix$stop[nrow(fix)]]))
    
  } else if (sac$start[1] == 1 & (sac$stop[nrow(sac)] + 1) < max(xy$time)) {
    # starts with saccade, ends with fixation/blink
    
    fix = data.frame(matrix(NA, nrow(sac), 9))
    colnames(fix) = c("num", "start", "stop", "xs", "ys", "xe", "ye", 
                      "xdrift", "ydrift")  
    
    # first fixation
    fix$num[1] <- 1
    fix$start[1] <- sac$stop[1] + 1
    fix$stop[1] = sac$start[1 + 1] - 1
    fix$xs[1] = round(median(xy$x[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$ys[1] = round(median(xy$y[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$xdrift[1] = round(MedianSD(xy$x[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$ydrift[1] = round(MedianSD(xy$y[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    
    if (nrow(fix) > 2) { # FIX: do only if more than two fixations
      
      # other fixations
      for (i in 2:(nrow(sac) - 1)){
        fix$num[i] <- i
        fix$start[i] <- sac$stop[i] + 1
        fix$stop[i] = sac$start[i + 1] - 1
        fix$xs[i] = round(median(xy$x[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$ys[i] = round(median(xy$y[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$xdrift[i] = round(MedianSD(xy$x[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$ydrift[i] = round(MedianSD(xy$y[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
      } 
    }
    
    # last fixation 
    fix$num[nrow(fix)] <- nrow(fix)
    fix$start[nrow(fix)] <-  sac$stop[nrow(sac)] + 1
    fix$stop[nrow(fix)] <-  max(xy$time)
    fix$xs[nrow(fix)] = round(median(xy$x[xy$time >= fix$start[nrow(fix)] & xy$time <= fix$stop[nrow(fix)]]))
    fix$ys[nrow(fix)] = round(median(xy$y[xy$time >= fix$start[nrow(fix)] & xy$time <= fix$stop[nrow(fix)]]))
    fix$xdrift[nrow(fix)] = round(MedianSD(xy$x[xy$time >= fix$start[nrow(fix)] & xy$time <= fix$stop[nrow(fix)]]))
    fix$ydrift[nrow(fix)] = round(MedianSD(xy$y[xy$time >= fix$start[nrow(fix)] & xy$time <= fix$stop[nrow(fix)]]))
    
  } else if (sac$start[1] > 1 & (sac$stop[nrow(sac)] + 1) == max(xy$time)) {
    # starts with fixation/blink, ends with saccade
    
    fix = data.frame(matrix(NA, nrow(sac), 9))
    colnames(fix) = c("num", "start", "stop", "xs", "ys", "xe", "ye", 
                      "xdrift", "ydrift")  
    
    # first fixation 
    fix$num[1] <- 1
    fix$start[1] <- 1
    fix$stop[1] <- sac$start[1] - 1
    fix$xs[1] <- round(median(xy$x[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$ys[1] <- round(median(xy$y[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$xdrift[1] <- round(MedianSD(xy$x[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$ydrift[1] <- round(MedianSD(xy$y[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    
    if (nrow(fix) > 2) { # FIX: do only if more than two fixations
      
      # other fixations
      for (i in 2:nrow(fix)){
        fix$num[i] <- i
        fix$start[i] <- sac$stop[i - 1] + 1
        fix$stop[i] = sac$start[i] - 1
        fix$xs[i] = round(median(xy$x[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$ys[i] = round(median(xy$y[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$xdrift[i] = round(MedianSD(xy$x[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$ydrift[i] = round(MedianSD(xy$y[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
      }
    }
    
  } else if (sac$start[1] == 1 & (sac$stop[nrow(sac)] + 1) == max(xy$time)) {
    # starts with saccade, ends with saccade
    
    fix = data.frame(matrix(NA, nrow(sac) - 1, 9))
    colnames(fix) = c("num", "start", "stop", "xs", "ys", "xe", "ye", 
                      "xdrift", "ydrift")  
    
    # first fixation
    fix$num[1] <- 1
    fix$start[1] <- sac$stop[1] + 1
    fix$stop[1] = sac$start[1 + 1] - 1
    fix$xs[1] = round(median(xy$x[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$ys[1] = round(median(xy$y[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$xdrift[1] = round(MedianSD(xy$x[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    fix$ydrift[1] = round(MedianSD(xy$y[xy$time >= fix$start[1] & xy$time <= fix$stop[1]]))
    
    if (nrow(fix) > 2) { # FIX: do only if more than two fixations
      
      # other fixations
      for (i in 2:(nrow(fix))){
        fix$num[i] <- i
        fix$start[i] <- sac$stop[i] + 1
        fix$stop[i] = sac$start[i + 1] - 1
        fix$xs[i] = round(median(xy$x[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$ys[i] = round(median(xy$y[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$xdrift[i] = round(MedianSD(xy$x[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
        fix$ydrift[i] = round(MedianSD(xy$y[xy$time >= fix$start[i] & xy$time <= fix$stop[i]]))
      } 
    }
    
  }
  
  # drift check
  #if (env$exp$setup$analysis$drift == TRUE) {
    for (i in 1:nrow(fix)) {
      if (fix$xdrift[i] > env$exp$setup$analysis$drift * env$exp$setup$font$size | 
          fix$ydrift[i] > env$exp$setup$analysis$drift * env$exp$setup$font$size) {
        fix$xs[i] <- NA
        fix$ys[i] <- NA
      }  
    }
    fix$xdrift <- NULL
    fix$ydrift <- NULL  
  #}
  
  return(fix)
  
}
