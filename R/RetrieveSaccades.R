
RetrieveSaccades <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial = 1
  
  # setup output
  dat$trial[[trial]]$sac <- 
    data.frame(matrix(NA, (nrow(dat$trial[[trial]]$fix) - 1), 12))
  colnames(dat$trial[[trial]]$sac) <- 
    c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg", "lines", "linee", 
      "lets", "lete")
  
  # extract saccades
  for (i in 1:(nrow(dat$trial[[trial]]$fix) - 1)){
    dat$trial[[trial]]$sac$num[i] <- i
    dat$trial[[trial]]$sac$start[i] <- dat$trial[[trial]]$fix$stop[i] + 1
    dat$trial[[trial]]$sac$stop[i] <- dat$trial[[trial]]$fix$start[i + 1] - 1
    dat$trial[[trial]]$sac$xs[i] <- dat$trial[[trial]]$fix$xs[i]
    dat$trial[[trial]]$sac$ys[i] <- dat$trial[[trial]]$fix$ys[i]
    dat$trial[[trial]]$sac$xe[i] <- dat$trial[[trial]]$fix$xs[i + 1]
    dat$trial[[trial]]$sac$ye[i] <- dat$trial[[trial]]$fix$ys[i + 1]
    dat$trial[[trial]]$sac$lines[i] <- dat$trial[[trial]]$fix$line[i]
    dat$trial[[trial]]$sac$linee[i] <- dat$trial[[trial]]$fix$line[i + 1]
    dat$trial[[trial]]$sac$lets[i] <- dat$trial[[trial]]$fix$line.let[i]
    dat$trial[[trial]]$sac$lete[i] <- dat$trial[[trial]]$fix$line.let[i + 1]
    dat$trial[[trial]]$sac$msg <- "SAC"
  }
  
  # check blinks
  blink <- dat$trial[[trial]]$parse[dat$trial[[trial]]$parse$msg == "BLINK", 1:7]
  for (i in 1:nrow(dat$trial[[trial]]$sac)) {
    if (dat$trial[[trial]]$sac$start[i] %in% blink$start) {
      dat$trial[[trial]]$sac$msg[i] <- "BLINK"
    }
  }
  
  # NOTE: deletes first saccade (if there is one)
  # NOTE: deletes last saccade (if there is one)
  
  
  # drift correct 
  # ---------------
  
  # x axis
  if (env$exp$setup$assign$driftX == T) {
    
    if (is.na(dat$trial[[trial]]$meta$drift) == F) {
      
      dat$trial[[trial]]$sac$xsn <- dat$trial[[trial]]$sac$xs - dat$trial[[trial]]$meta$drift.x
      dat$trial[[trial]]$sac$xen <- dat$trial[[trial]]$sac$xe - dat$trial[[trial]]$meta$drift.x
      
    } else {
      
      dat$trial[[trial]]$sac$xsn <- dat$trial[[trial]]$sac$xs
      dat$trial[[trial]]$sac$xen <- dat$trial[[trial]]$sac$xe
    }
    
  } else {
    
    dat$trial[[trial]]$sac$xsn <- dat$trial[[trial]]$sac$xs
    dat$trial[[trial]]$sac$xen <- dat$trial[[trial]]$sac$xe
    
  }
  
  # y axis
  if (env$exp$setup$assign$driftY == T) {
    
    if (is.na(dat$trial[[trial]]$meta$drift) == F) {
      
      dat$trial[[trial]]$sac$ysn <- dat$trial[[trial]]$sac$ys - dat$trial[[trial]]$meta$drift.y + env$exp$setup$font$height / 2
      dat$trial[[trial]]$sac$yen <- dat$trial[[trial]]$sac$ye - dat$trial[[trial]]$meta$drift.y + env$exp$setup$font$height / 2
      
    } else {
      
      dat$trial[[trial]]$sac$ysn <- dat$trial[[trial]]$sac$ys
      dat$trial[[trial]]$sac$yen <- dat$trial[[trial]]$sac$ye
      
    }
    
  } else {
    
    dat$trial[[trial]]$sac$ysn <- dat$trial[[trial]]$sac$ys
    dat$trial[[trial]]$sac$yen <- dat$trial[[trial]]$sac$ye
    
  }
  
  return(dat)
  
}
