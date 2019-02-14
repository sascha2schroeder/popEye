
RetrieveFixations <- function(dat, trial) {

  dat$trial[[trial]]$fix <- 
    dat$trial[[trial]]$parse[dat$trial[[trial]]$parse$msg == "FIX", ]
  dat$trial[[trial]]$fix$xe <- NULL
  dat$trial[[trial]]$fix$ye <- NULL
  dat$trial[[trial]]$fix$msg <- NULL
  dat$trial[[trial]]$fix$num <- 1:nrow(dat$trial[[trial]]$fix)
  
  return(dat)
  
}
