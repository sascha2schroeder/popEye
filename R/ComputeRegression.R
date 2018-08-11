
ComputeRegression <- function(dat, trial) {
  
  # initialize
  dat$trial[[trial]]$fix$word.reg.out <- 0
  dat$trial[[trial]]$fix$word.reg.in <- 0
  
  dat$trial[[trial]]$fix$ia.reg.out <- 0
  dat$trial[[trial]]$fix$ia.reg.in <- 0
  
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    # j <- 2
    
    # word
    if(dat$trial[[trial]]$fix$wordnum[j] < dat$trial[[trial]]$fix$wordnum[j - 1]) {
      dat$trial[[trial]]$fix$word.reg.in[j] <- 1
      dat$trial[[trial]]$fix$word.reg.out[j - 1] <- 1
    }
    
    # IA
    if(dat$trial[[trial]]$fix$ianum[j] < dat$trial[[trial]]$fix$ianum[j - 1]) {
      dat$trial[[trial]]$fix$ia.reg.in[j] <- 1
      dat$trial[[trial]]$fix$ia.reg.out[j - 1] <- 1
    }
    
  }
  
  return(dat)
  
}
