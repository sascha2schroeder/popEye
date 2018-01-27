
ComputeSaccadeLength <- function(dat, trial) {
  
  # trial = 1
    
  # incoming
  dat$trial[[trial]]$fix$sac.in <- NA
  dat$trial[[trial]]$fix$sac.in[1] <- dat$trial[[trial]]$fix$letter[1] 
  for (j in 2:nrow(dat$trial[[trial]]$fix)) {
    dat$trial[[trial]]$fix$sac.in[j] <- dat$trial[[trial]]$fix$letter[j] - 
      dat$trial[[trial]]$fix$letter[j - 1]
  }
  
  # outgoing
  dat$trial[[trial]]$fix$sac.out <- NA
  for (j in 1:(nrow(dat$trial[[trial]]$fix) - 1)){
    dat$trial[[trial]]$fix$sac.out[j] <- dat$trial[[trial]]$fix$letter[j + 1] - 
      dat$trial[[trial]]$fix$letter[j] 
  }
  
  return(dat)
  
}