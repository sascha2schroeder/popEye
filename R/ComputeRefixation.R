
ComputeRefixation <- function(dat, trial) {
  
  dat$trial[[trial]]$fix$refix <- 0
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    # j <- 2
    if(dat$trial[[trial]]$fix$ia[j] == dat$trial[[trial]]$fix$ia[j - 1]) {
      dat$trial[[trial]]$fix$refix[j] <- 1
    }
  }
  
  return(dat)
  
}  