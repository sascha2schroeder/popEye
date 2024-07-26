
ComputeSaccadeMeasures <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 1
  
  # print(trial)
  
  sac <- dat$item[[trial]]$sac
  
  sac$subid <- env$subid
  sac$trialid <- dat$item[[trial]]$meta$trialid
  sac$trialnum <- dat$item[[trial]]$meta$trialnum
  sac$itemid <- dat$item[[trial]]$meta$itemid
  sac$cond <- dat$item[[trial]]$meta$cond
  
  sac$peak.vel <- NA
  sac$dx <- NA
  sac$dy <- NA
  sac$dX <- NA
  sac$dY <- NA
  sac$dist.let <- NA
  sac$dist.px <- NA
  sac$dist.angle <- NA
  sac$amp.px <- NA
  sac$amp.angle <- NA
  
  for (s in 1:nrow(sac)) {
   # s = 1
    
    # print(s)
    
    # on-/offset
    a <- sac$start[s]
    b <- sac$stop[s]
    
    # duration
    sac$dur[s] <- b - a + 1
    
    # saccade distance in letters
    sac$dist.let[s] <- sac$lete[s] - sac$lets[s]
    
    # treat negative positions as missing
    dat$item[[trial]]$xy$x[dat$item[[trial]]$xy$x < 0] <- NA
    dat$item[[trial]]$xy$y[dat$item[[trial]]$xy$y < 0] <- NA
    
    # print(dat$item[[trial]]$xy$x[dat$item[[trial]]$xy$time >= a & dat$item[[trial]]$xy$time <= b])
    # print(dat$item[[trial]]$xy$y[dat$item[[trial]]$xy$time >= a & dat$item[[trial]]$xy$time <= b])
    
    if (length(dat$item[[trial]]$xy$x[dat$item[[trial]]$xy$time >= a & dat$item[[trial]]$xy$time <= b]) > 0 & 
        sum(is.na(dat$item[[trial]]$xy$x[dat$item[[trial]]$xy$time >= a & dat$item[[trial]]$xy$time <= b])) == 0 &
        length(dat$item[[trial]]$xy$y[dat$item[[trial]]$xy$time >= a & dat$item[[trial]]$xy$time <= b]) > 0 & 
        sum(is.na(dat$item[[trial]]$xy$y[dat$item[[trial]]$xy$time >= a & dat$item[[trial]]$xy$time <= b])) == 0) {
      
      # saccade distance px (dx, dy)
      sac$dx[s] <- sac$xen[s] - sac$xsn[s]
      
      if (dat$item[[trial]]$meta$calibration.method != "H3") {
        sac$dy[s] <- sac$yen[s] - sac$ysn[s]
      }
      
      # saccade distance/angle
      if (dat$item[[trial]]$meta$calibration.method != "H3") {
        sac$dist.px[s] <- round(sqrt(sac$dx[s]^2 + sac$dy[s]^2))
        sac$dist.angle[s] <- round(atan2(sac$dy[s], sac$dx[s]), 2)
      } else {
        sac$dist.px[s] <- round(sac$dx[s])
        sac$dist.angle[s] <- round(atan(sac$dx[s]), 2)
      }
      
      if (is.na(dat$item[[trial]]$meta$calibration.method) == F) {
        
        # saccade peak velocity (vpeak)
        if (dat$item[[trial]]$meta$calibration.method != "H3") {
          sac$peak.vel[s] <- round(max(sqrt(dat$item[[trial]]$vxy$x[dat$item[[trial]]$vxy$time >= a &
                                                                      dat$item[[trial]]$vxy$time <= b]^2
                                            + dat$item[[trial]]$vxy$y[dat$item[[trial]]$vxy$time >= a &
                                                                        dat$item[[trial]]$vxy$time <= b]^2),
                                       na.rm = T))
        } else {
          sac$peak.vel[s] <- round(max(sqrt(dat$item[[trial]]$vxy$x[dat$item[[trial]]$vxy$time >= a &
                                                                      dat$item[[trial]]$vxy$time <= b]^2),
                                       na.rm = T))
        }
        
        # saccade amplitude (dX, dY)
        minx <- min(dat$item[[trial]]$xy[dat$item[[trial]]$xy$time >= a
                                         & dat$item[[trial]]$xy$time <= b, 2])
        maxx <- max(dat$item[[trial]]$xy[dat$item[[trial]]$xy$time >= a
                                         & dat$item[[trial]]$xy$time <= b, 2])
        if (dat$item[[trial]]$meta$calibration.method != "H3") {
          miny <- min(dat$item[[trial]]$xy[dat$item[[trial]]$xy$time >= a
                                           & dat$item[[trial]]$xy$time <= b, 3])
          maxy <- max(dat$item[[trial]]$xy[dat$item[[trial]]$xy$time >= a
                                           & dat$item[[trial]]$xy$time <= b, 3])
        }
        ix1 <- which.min(dat$item[[trial]]$xy[dat$item[[trial]]$xy$time >= a
                                              & dat$item[[trial]]$xy$time <= b, 2])
        ix2 <- which.max(dat$item[[trial]]$xy[dat$item[[trial]]$xy$time >= a
                                              & dat$item[[trial]]$xy$time <= b, 2])
        if (dat$item[[trial]]$meta$calibration.method != "H3") {
          iy1 <- which.min(dat$item[[trial]]$xy[dat$item[[trial]]$xy$time >= a
                                                & dat$item[[trial]]$xy$time <= b, 3])
          iy2 <- which.max(dat$item[[trial]]$xy[dat$item[[trial]]$xy$time >= a
                                                & dat$item[[trial]]$xy$time <= b, 3])
        }
        sac$dX[s] <- round(sign(ix2 - ix1) * (maxx - minx))
        if (dat$item[[trial]]$meta$calibration.method != "H3") {
          sac$dY[s] <- round(sign(iy2 - iy1) * (maxy - miny))
        }
        
        # saccade amplitude/angle
        if (dat$item[[trial]]$meta$calibration.method != "H3" ) {
          sac$amp.px[s] <- round(sqrt(sac$dX[s]^2 + sac$dY[s]^2))
          sac$amp.angle[s] <- round(atan2(sac$dY[s], sac$dX[s]), 2)
        } else {
          sac$amp.px[s] <- sac$dX[s]
          sac$amp.angle[s] <- round(atan(sac$dX[s]), 2)
        }
        
      }
      
    } else {
      
      sac$msg[s] <- "BLINK"
      
    }
    
  }
  
  # rename fixid
  sac$sacid <- sac$num
  
  # names and return
  
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "sacid", "msg",
             "xs", "xe", "ys", "ye", "xsn", "xen", "ysn", "yen", "start", "stop", "dur",
             "lines", "linee", "lets", "lete", "dist.let", 
             "dx", "dy", "dist.px", "dist.angle", 
             "dX", "dY", "amp.px", "amp.angle", "peak.vel")

  sac <- sac[names]
  
  
  # treat very long saccades as blinks (controlled by exclude.sac)
  sac$msg[sac$dur > env$exp$setup$exclude$sac] <- "BLINK"
  names <- c("dx", "dy", "dist.px", "dist.angle", "dX", "dY", "amp.px", "amp.angle", "peak.vel")
  sac[sac$dur > env$exp$setup$exclude$sac, names] <- NA
  


  dat$item[[trial]]$sac <- sac[is.na(sac$start) == F, ]
  
  return(dat)  
  
}
