
ComputeLaunchDistance <- function(dat, trial) {
  
  # word
  dat$trial[[trial]]$fix$word.launch <- dat$trial[[trial]]$fix$sac.in - 
    dat$trial[[trial]]$fix$word.land
  # IA
  dat$trial[[trial]]$fix$ia.launch <- dat$trial[[trial]]$fix$sac.in - 
    dat$trial[[trial]]$fix$ia.land
  
  return (dat)
  
}
