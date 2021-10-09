
ComputeRefixation <- function(dat, trial) {
  
  # initialize
  dat$item[[trial]]$fix$word.refix <- 0
  dat$item[[trial]]$fix$ia.refix <- 0
  dat$item[[trial]]$fix$sent.refix <- 0
  
  for (j in 2:nrow(dat$item[[trial]]$fix)){
    # j <- 2
    
    # dat$item[[trial]]$fix$word.refix[1] <- 0
    # dat$item[[trial]]$fix$ia.refix[1] <- 0
    # dat$item[[trial]]$fix$sent.refix[1] <- 0
    
    # skip outliers
    if(is.na(dat$item[[trial]]$fix$wordnum[j]) | is.na(dat$item[[trial]]$fix$wordnum[j - 1])) next
    # NOTE: delete if outliers are excluded earlier
    
    # word
    if(dat$item[[trial]]$fix$wordnum[j] == dat$item[[trial]]$fix$wordnum[j - 1]) {
      dat$item[[trial]]$fix$word.refix[j] <- 1
    } else {
      dat$item[[trial]]$fix$word.refix[j] <- 0
    }
    
    # IA
    if(dat$item[[trial]]$fix$ianum[j] == dat$item[[trial]]$fix$ianum[j - 1]) {
      dat$item[[trial]]$fix$ia.refix[j] <- 1
    } else {
      dat$item[[trial]]$fix$ia.refix[j] <- 0
    }
    
    # sent
    if(dat$item[[trial]]$fix$sentnum[j] == dat$item[[trial]]$fix$sentnum[j - 1]) {
      dat$item[[trial]]$fix$sent.refix[j] <- 1
    } else {
      dat$item[[trial]]$fix$sent.refix[j] <- 0
    }
    
  }
  
  return(dat)
  
}  