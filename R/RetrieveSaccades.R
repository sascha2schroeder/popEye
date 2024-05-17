
RetrieveSaccades <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial = 1
  
  # setup output
  dat$item[[trial]]$sac <- 
    data.frame(matrix(NA, (nrow(dat$item[[trial]]$fix) - 1), 12))
  colnames(dat$item[[trial]]$sac) <- 
    c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg", "lines", "linee", 
      "lets", "lete")
  
  # extract saccades
  for (i in 1:(nrow(dat$item[[trial]]$fix) - 1)){
    dat$item[[trial]]$sac$num[i] <- i
    dat$item[[trial]]$sac$start[i] <- dat$item[[trial]]$fix$stop[i] + 1
    dat$item[[trial]]$sac$stop[i] <- dat$item[[trial]]$fix$start[i + 1] - 1
    dat$item[[trial]]$sac$xs[i] <- dat$item[[trial]]$fix$xs[i]
    dat$item[[trial]]$sac$ys[i] <- dat$item[[trial]]$fix$ys[i]
    dat$item[[trial]]$sac$xe[i] <- dat$item[[trial]]$fix$xs[i + 1]
    dat$item[[trial]]$sac$ye[i] <- dat$item[[trial]]$fix$ys[i + 1]
    dat$item[[trial]]$sac$lines[i] <- dat$item[[trial]]$fix$line[i]
    dat$item[[trial]]$sac$linee[i] <- dat$item[[trial]]$fix$line[i + 1]
    dat$item[[trial]]$sac$lets[i] <- dat$item[[trial]]$fix$line.let[i]
    dat$item[[trial]]$sac$lete[i] <- dat$item[[trial]]$fix$line.let[i + 1]
    dat$item[[trial]]$sac$msg <- "SAC"
  }
  
  # check blinks
  blink <- dat$item[[trial]]$parse[dat$item[[trial]]$parse$msg == "BLINK", 1:7]
  for (i in 1:nrow(dat$item[[trial]]$sac)) {
    if (dat$item[[trial]]$sac$start[i] %in% blink$start) {
      dat$item[[trial]]$sac$msg[i] <- "BLINK"
    }
  }
  
  # NOTE: deletes first saccade (if there is one)
  # NOTE: deletes last saccade (if there is one)
  
  
  # drift correct 
  # ---------------
  
  # x axis
  if (env$exp$setup$assign$driftX == T) {
    
    if (is.na(dat$item[[trial]]$meta$drift) == F) {
      
      dat$item[[trial]]$sac$xsn <- dat$item[[trial]]$sac$xs - dat$item[[trial]]$meta$drift.x
      dat$item[[trial]]$sac$xen <- dat$item[[trial]]$sac$xe - dat$item[[trial]]$meta$drift.x
      
    } else {
      
      dat$item[[trial]]$sac$xsn <- dat$item[[trial]]$sac$xs
      dat$item[[trial]]$sac$xen <- dat$item[[trial]]$sac$xe
    }
    
  } else {
    
    dat$item[[trial]]$sac$xsn <- dat$item[[trial]]$sac$xs
    dat$item[[trial]]$sac$xen <- dat$item[[trial]]$sac$xe
    
  }
  
  # y axis
  if (env$exp$setup$assign$driftY == T) {
    
    if (is.na(dat$item[[trial]]$meta$drift) == F) {
      
      dat$item[[trial]]$sac$ysn <- dat$item[[trial]]$sac$ys - dat$item[[trial]]$meta$drift.y + env$exp$setup$font$height / 2
      dat$item[[trial]]$sac$yen <- dat$item[[trial]]$sac$ye - dat$item[[trial]]$meta$drift.y + env$exp$setup$font$height / 2
      
    } else {
      
      dat$item[[trial]]$sac$ysn <- dat$item[[trial]]$sac$ys
      dat$item[[trial]]$sac$yen <- dat$item[[trial]]$sac$ye
      
    }
    
  } else {
    
    dat$item[[trial]]$sac$ysn <- dat$item[[trial]]$sac$ys
    dat$item[[trial]]$sac$yen <- dat$item[[trial]]$sac$ye
    
  }
  
  return(dat)
  
}
