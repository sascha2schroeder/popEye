
ComputeSaccadeMeasures <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 1
  
  sac <- dat$trial[[trial]]$sac
  
  sac$subid <- env$subid
  sac$trialid <- dat$trial[[trial]]$meta$trialid
  sac$trialnum <- dat$trial[[trial]]$meta$trialnum
  sac$itemid <- dat$trial[[trial]]$meta$itemid
  sac$cond <- dat$trial[[trial]]$meta$cond
  
  sac$peak.vel <- NA
  sac$dx <- NA
  sac$dy <- NA
  sac$dX <- NA
  sac$dY <- NA
  sac$dist.px <- NA
  sac$dist.angle <- NA
  sac$amp.px <- NA
  sac$amp.angle <- NA
  
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
      if (dat$trial[[trial]]$meta$calibration.method != "H3") {
        sac$peak.vel[s] <- round(max(sqrt(dat$trial[[trial]]$vxy$x[a:b]^2
                                          + dat$trial[[trial]]$vxy$y[a:b]^2), 
                                     na.rm = T))
      } else {
        sac$peak.vel[s] <- round(max(sqrt(dat$trial[[trial]]$vxy$x[a:b]^2), 
                                     na.rm = T))
      }
      
      # saccade distance (dx, dy)
      sac$dx[s] <- round(dat$trial[[trial]]$xy[b, 2] - dat$trial[[trial]]$xy[a, 2])
      if (dat$trial[[trial]]$meta$calibration.method != "H3") {
        sac$dy[s] <- round(dat$trial[[trial]]$xy[b, 3] - dat$trial[[trial]]$xy[a, 3])
      }
      
      # saccade amplitude (dX, dY)
      i <- sac[s, 2]:sac[s, 3]
      minx <- min(dat$trial[[trial]]$xy[i, 2])
      maxx <- max(dat$trial[[trial]]$xy[i, 2])
      if (dat$trial[[trial]]$meta$calibration.method != "H3") {
        miny <- min(dat$trial[[trial]]$xy[i, 3])
        maxy <- max(dat$trial[[trial]]$xy[i, 3])
      }
      ix1 <- which.min(dat$trial[[trial]]$xy[i, 2])
      ix2 <- which.max(dat$trial[[trial]]$xy[i, 2])
      if (dat$trial[[trial]]$meta$calibration.method != "H3") {
        iy1 <- which.min(dat$trial[[trial]]$xy[i, 3])
        iy2 <- which.max(dat$trial[[trial]]$xy[i, 3])
      }
      sac$dX[s] <- round(sign(ix2 - ix1) * (maxx - minx))
      if (dat$trial[[trial]]$meta$calibration.method != "H3") {
        sac$dY[s] <- round(sign(iy2 - iy1) * (maxy - miny))
      }
      
      # saccade distance/angle
      if (dat$trial[[trial]]$meta$calibration.method != "H3") {
        sac$dist.px[s] <- round(sqrt(sac$dx[s]^2 + sac$dy[s]^2))
        sac$dist.angle[s] <- round(atan2(sac$dy[s], sac$dx[s]), 2)
      } else {
        sac$dist.px[s] <- NA
        sac$dist.angle[s] <- NA
      }
      
      # saccade amplitude/angle
      if (dat$trial[[trial]]$meta$calibration.method != "H3") {
        sac$amp.px[s] <- round(sqrt(sac$dX[s]^2 + sac$dY[s]^2))
        sac$amp.angle[s] <- round(atan2(sac$dY[s], sac$dX[s]), 2)
      } else {
        sac$amp.px[s] <- NA
        sac$amp.angle[s] <- NA
      }
      
    }
    
    # saccade distance in letters
    sac$dist.let[s] <- sac$lete[s] - sac$lets[s]
    
    # duration
    sac$dur[s] <- round((b - a + 1) * 1000 / env$exp$setup$tracker$samp)
    
  }
  
  # rename fixid
  sac$sacid <- sac$num
  
  # names and return
    names <- c("subid", "trialid", "trialnum", "itemid", "cond", "sacid", "msg", 
               "xs", "xe", "ys", "ye", "xsn", "xen", "ysn", "yen", "start", "stop", 
               "dist.px", "dist.let", "peak.vel", "dur")
  sac <- sac[names]
  
  # treat very long saccades as blinks (controlled by exclude.sac)
  sac$msg[sac$dur > env$exp$setup$exclude$sac] <- "BLINK"
  
  dat$trial[[trial]]$sac <- sac[is.na(sac$start) == F, ]
  
  
  return(dat)  
  
}
