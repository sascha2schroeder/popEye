
ComputeLaunchDistance <- function(dat, trial) {
  
  dat$trial[[trial]]$fix$launch <- dat$trial[[trial]]$fix$sac.in - 
    dat$trial[[trial]]$fix$land
  
  return (dat)
  
}
