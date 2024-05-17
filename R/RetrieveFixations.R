
RetrieveFixations <- function(dat, trial) {

  dat$item[[trial]]$fix <- 
    dat$item[[trial]]$parse[dat$item[[trial]]$parse$msg == "FIX", ]
  dat$item[[trial]]$fix$xe <- NULL
  dat$item[[trial]]$fix$ye <- NULL
  dat$item[[trial]]$fix$msg <- NULL
  dat$item[[trial]]$fix$num <- 1:nrow(dat$item[[trial]]$fix)
  
  return(dat)
  
}
