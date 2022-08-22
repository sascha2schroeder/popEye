
ComputeDur <- function(dat, trial) {
  dat$item[[trial]]$fix$dur <- dat$item[[trial]]$fix$stop - 
    dat$item[[trial]]$fix$start + 1
  
  return(dat)

}