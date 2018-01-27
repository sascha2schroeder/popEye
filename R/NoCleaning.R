
# NoCleaning
# -----------------

NoCleaning <- function(dat, trial) {
  
  dat$trial[[trial]]$fix <- dat$trial[[trial]]$fix
  
  return(dat)
}
