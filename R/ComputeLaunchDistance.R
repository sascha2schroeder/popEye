
ComputeLaunchDistance <- function(dat, trial) {
  
  # word
  dat$item[[trial]]$fix$word.launch <- dat$item[[trial]]$fix$sac.in - 
    dat$item[[trial]]$fix$word.land
  # IA
  dat$item[[trial]]$fix$ia.launch <- dat$item[[trial]]$fix$sac.in - 
    dat$item[[trial]]$fix$ia.land
  
  return (dat)
  
}
