
ComputeEvents <- function(xy, vxy) {
  
  
  # computes saccades, fixations, and blinks
  # -----------------------------------------
  
  # SAC
  sac <- ComputeSaccades(xy, vxy)$sac
  
  
  # FIX: check number of saccades
  # ------------------------------
  if (sum(sac$start > 0) == 0) {
    fix = data.frame(matrix(0, 1, 9))
    colnames(fix) = c("num", "start", "stop", "xs", "ys", "xe", "ye", 
                      "xdrift", "ydrift")  
  } else {
    # FIX
    fix <- ComputeFixations(xy, sac)
  }
  
  # BLINK
  blink <- fix[is.na(fix$xs) == T, ]
  
  
  # integrate blinks
  # -----------------
 
  # NOTE: Not sure whether this works with multiple blinks in a trial
  
  out <- list(sac = sac, fix = fix, blink = blink)
  
  if (nrow(out$blink) > 0) {
    for (i in 1:nrow(out$blink)) {
      if (out$blink$num[i] > max(out$sac$num)){
        out$sac$stop[max(out$sac$num)] <- out$blink$stop[which.max(out$blink$num)]
        out$sac$xs[max(out$sac$num)] <- NA
        out$sac$ys[max(out$sac$num)] <- NA
        out$sac$xe[max(out$sac$num)] <- NA
        out$sac$ye[max(out$sac$num)] <- NA
      } else {
        out$sac$stop[out$sac$num == (out$blink$num[i] - 1)] <- out$sac$stop[out$sac$num == out$blink$num[i]] 
        out$sac$xe[out$sac$num == (out$blink$num[i] - 1)] <- out$sac$xe[sac$num == out$blink$num[i]] 
        out$sac$ye[out$sac$num == (out$blink$num[i] - 1)] <- out$sac$ye[sac$num == out$blink$num[i]]   
      }
    }
    
    out$sac <- out$sac[-out$blink$num, ]
    out$fix <- out$fix[is.na(out$fix$xs) == F, ]
    
    out$fix <- out$fix[(out$fix$stop - out$fix$start) > 0,]
    out$sac <- out$sac[(out$sac$stop - out$sac$start) > 0,]
    out$blink <- out$blink[(out$blink$stop - out$blink$start) > 0,]
    
    out$fix$num <- 1:nrow(out$fix)
    out$sac$num <- 1:nrow(out$sac)
    out$blink$num <- 1:nrow(out$blink)
    row.names(out$sac) <- NULL
    row.names(out$fix) <- NULL
    row.names(out$blink) <- NULL
    
  }

  return(out)
  
}
