
ComputeRegression <- function(dat, trial) {
  
  dat$trial[[trial]]$fix$reg.out <- 0
  dat$trial[[trial]]$fix$reg.in <- 0
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    # j <- 2
    if(dat$trial[[trial]]$fix$ia[j] < dat$trial[[trial]]$fix$ia[j - 1]) {
      dat$trial[[trial]]$fix$reg.in[j] <- 1
      dat$trial[[trial]]$fix$reg.out[j - 1] <- 1
    }
  }
  
  return(dat)
  
}
