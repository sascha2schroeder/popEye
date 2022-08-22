
ComputeLaunchDistance <- function(dat, trial) {
  
  dat$item[[trial]]$fix$word.launch <- NA
  dat$item[[trial]]$fix$ia.launch <- NA
  
  for (i in 2:nrow(dat$item[[trial]]$fix)) {
    
    if(is.na(dat$item[[trial]]$fix$sac.in[i])) {
      next
    }
    
    if (dat$item[[trial]]$fix$sac.in[i] >= 0) {
      
      # word
      dat$item[[trial]]$fix$word.launch[i] <- dat$item[[trial]]$fix$sac.in[i] - 
        dat$item[[trial]]$fix$word.land[i]
      
      # IA
      dat$item[[trial]]$fix$ia.launch[i] <- dat$item[[trial]]$fix$sac.in[i] - 
        dat$item[[trial]]$fix$ia.land[i]
      
    } else {
      
      # word
      dat$item[[trial]]$fix$word.launch[i] <- dat$item[[trial]]$fix$sac.in[i] + 
        dat$item[[trial]]$fix$word.land[i -1]
      
      # IA
      dat$item[[trial]]$fix$ia.launch[i] <- dat$item[[trial]]$fix$sac.in[i] + 
        dat$item[[trial]]$fix$ia.land[i - 1]
      
    }
    
  }
  
  return (dat)
  
}
