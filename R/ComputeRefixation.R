
ComputeRefixation <- function(dat, trial) {
  
  # initialize
  dat$trial[[trial]]$fix$word.refix <- 0
  dat$trial[[trial]]$fix$ia.refix <- 0
  
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    # j <- 2
    
    # word
    if(dat$trial[[trial]]$fix$wordnum[j] == dat$trial[[trial]]$fix$wordnum[j - 1]) {
      dat$trial[[trial]]$fix$word.refix[j] <- 1
    }
    
    # IA
    if(dat$trial[[trial]]$fix$ianum[j] == dat$trial[[trial]]$fix$ianum[j - 1]) {
      dat$trial[[trial]]$fix$ia.refix[j] <- 1
    }
  }
  
  return(dat)
  
}  