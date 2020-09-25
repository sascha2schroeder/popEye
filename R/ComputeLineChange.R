
ComputeLineChange <- function(dat, trial) {
  
  # trial = 1
  
  tmp <- dat$item[[trial]]$fix[dat$item[[trial]]$fix$type == "in", ]
  
  if (nrow(tmp) > 1) {
    
    tmp$line.change <- NA
    tmp$line.change[1] <- 0
    
    for (j in 2:nrow(tmp)) {
      
      if (is.na(tmp$line)) next
      tmp$line.change[j] <- tmp$line[j] - tmp$line[j - 1] 
      
    }  
    
  } else {
    
    tmp <- dat$item[[trial]]$fix
    tmp$line.change <- NA
    
  }
  
  names <- c("num", "line.change")
  out <- tmp[names]
  
  dat$item[[trial]]$fix <- merge(dat$item[[trial]]$fix, out, by = "num", all.x = T)
  return(dat)
  
}