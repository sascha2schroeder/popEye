
ComputeLineChange <- function(dat, trial) {
  
  # trial = 1
  
  tmp <- dat$trial[[trial]]$fix[dat$trial[[trial]]$fix$type == "in", ]
  
  if (nrow(tmp) > 1) {
    
    tmp$line.change <- NA
    tmp$line.change[1] <- 0
    
    for (j in 2:nrow(tmp)) {
      
      if (is.na(tmp$line)) next
      tmp$line.change[j] <- tmp$line[j] - tmp$line[j - 1] 
      
    }  
    
  } else {
    
    tmp <- dat$trial[[trial]]$fix
    tmp$line.change <- NA
    
  }
  
  names <- c("num", "line.change")
  out <- tmp[names]
  
  dat$trial[[trial]]$fix <- merge(dat$trial[[trial]]$fix, out, by = "num", all.x = T)
  return(dat)
  
}