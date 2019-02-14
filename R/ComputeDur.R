
ComputeDur <- function(dat, trial) {
  dat$trial[[trial]]$fix$dur <- dat$trial[[trial]]$fix$stop - 
    dat$trial[[trial]]$fix$start + 1
  
  return(dat)

}