
ComputeSaccadeMeasures <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 1
  
  sac <- dat$trial[[trial]]$sac
  sac$peak.vel <- NA
  
  for (s in 1:nrow(sac)) {
    # s = 1
    
    # on-/offset
    a <- sac$start[s]
    b <- sac$stop[s]
    
    # saccade peak velocity (vpeak)
    if (env$exp$setup$tracker$calibration != "H3") {
      sac$peak.vel[s] <- round(max( sqrt(dat$trial[[trial]]$vxy$x[a:b]^2 + dat$trial[[trial]]$vxy$y[a:b]^2 )))
    }
    
    # saccade distance (dx, dy)
    sac$dx[s] <- round(dat$trial[[trial]]$xy[b, 1] - dat$trial[[trial]]$xy[a, 1])
    if (env$exp$setup$tracker$calibration != "H3") {
      sac$dy[s] <- round(dat$trial[[trial]]$xy[b, 2] - dat$trial[[trial]]$xy[a, 2])
    }
    
    # saccade amplitude (dX, dY)
    i <- sac[s, 2]:sac[s, 3]
    minx <- min(dat$trial[[trial]]$xy[i, 1])
    maxx <- max(dat$trial[[trial]]$xy[i, 1])
    if (env$exp$setup$tracker$calibration != "H3") {
      miny <- min(dat$trial[[trial]]$xy[i, 2])
      maxy <- max(dat$trial[[trial]]$xy[i, 2])
    }
    ix1 <- which.min(dat$trial[[trial]]$xy[i, 1])
    ix2 <- which.max(dat$trial[[trial]]$xy[i, 1])
    if (env$exp$setup$tracker$calibration != "H3") {
      iy1 <- which.min(dat$trial[[trial]]$xy[i, 2])
      iy2 <- which.max(dat$trial[[trial]]$xy[i, 2])
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
    
    # duration
    sac$dur[s] <- round((b - a + 1) * 1000 / env$exp$setup$tracker$samp)
    
  }
  
  dat$trial[[trial]]$sac <- sac
  
  return(dat)  
  
}
