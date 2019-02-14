
ComputeSaccadeMeasures <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 1
  
  sac <- dat$trial[[trial]]$sac
  sac$peak.vel <- NA
  
  for (s in 1:nrow(sac)) {
    # s = 1
    
    # on-/offset
    a <- sac$start[s]
    b <- sac$stop[s]
    
    if (is.null(dat$trial[[trial]]$xy$x[a:b]) == TRUE) {
        dat$trial[[trial]]$xy$x[a:b] <- NA
    }
    
    if (is.na(dat$trial[[trial]]$xy$x[a:b]) == FALSE) {
      
      # saccade peak velocity (vpeak)
      if (env$exp$setup$tracker$calibration != "H3") {
        sac$peak.vel[s] <- round(max(sqrt(dat$trial[[trial]]$vxy$x[a:b]^2
                                          + dat$trial[[trial]]$vxy$y[a:b]^2), 
                                     na.rm = T))
      } else {
        sac$peak.vel[s] <- round(max(sqrt(dat$trial[[trial]]$vxy$x[a:b]^2), 
                                     na.rm = T))
      }
      
      # saccade distance (dx, dy)
      sac$dx[s] <- round(dat$trial[[trial]]$xy[b, 2] - dat$trial[[trial]]$xy[a, 2])
      if (env$exp$setup$tracker$calibration != "H3") {
        sac$dy[s] <- round(dat$trial[[trial]]$xy[b, 3] - dat$trial[[trial]]$xy[a, 3])
      }
      
      # saccade amplitude (dX, dY)
      i <- sac[s, 2]:sac[s, 3]
      minx <- min(dat$trial[[trial]]$xy[i, 2])
      maxx <- max(dat$trial[[trial]]$xy[i, 2])
      if (env$exp$setup$tracker$calibration != "H3") {
        miny <- min(dat$trial[[trial]]$xy[i, 3])
        maxy <- max(dat$trial[[trial]]$xy[i, 3])
      }
      ix1 <- which.min(dat$trial[[trial]]$xy[i, 2])
      ix2 <- which.max(dat$trial[[trial]]$xy[i, 2])
      if (env$exp$setup$tracker$calibration != "H3") {
        iy1 <- which.min(dat$trial[[trial]]$xy[i, 3])
        iy2 <- which.max(dat$trial[[trial]]$xy[i, 3])
      }
      sac$dX[s] <- round(sign(ix2 - ix1) * (maxx - minx))
      if (env$exp$setup$tracker$calibration != "H3") {
        sac$dY[s] <- round(sign(iy2 - iy1) * (maxy - miny))
      }
      # saccade distance/angle
      if (env$exp$setup$tracker$calibration != "H3") {
        sac$dist.px[s] <- round(sqrt(sac$dx[s]^2 + sac$dy[s]^2))
        sac$dist.angle[s] <- round(atan2(sac$dy[s], sac$dx[s]), 2)
      }
      
      # saccade amplitude/angle
      if (env$exp$setup$tracker$calibration != "H3") {
        sac$amp.px[s] <- round(sqrt(sac$dX[s]^2 + sac$dY[s]^2))
        sac$amp.angle[s] <- round(atan2(sac$dY[s], sac$dX[s]), 2)
      }
      
    }
    
    # duration
    sac$dur[s] <- round((b - a + 1) * 1000 / env$exp$setup$tracker$samp)
    
  }
  
  dat$trial[[trial]]$sac <- sac
  
  return(dat)  
  
}
