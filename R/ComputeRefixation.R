
ComputeRefixation <- function(dat, trial) {
  
  # initialize
  dat$trial[[trial]]$fix$word.refix <- NA
  dat$trial[[trial]]$fix$ia.refix <- NA
  dat$trial[[trial]]$fix$sent.refix <- NA
  
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    # j <- 2
    
    dat$trial[[trial]]$fix$word.refix[1] <- 0
    dat$trial[[trial]]$fix$ia.refix[1] <- 0
    dat$trial[[trial]]$fix$sent.refix[1] <- 0
    
    # skip outliers
    if(is.na(dat$trial[[trial]]$fix$wordnum[j]) | is.na(dat$trial[[trial]]$fix$wordnum[j - 1])) next
    # NOTE: delete if outliers are excluded earlier
    
    # word
    if(dat$trial[[trial]]$fix$wordnum[j] == dat$trial[[trial]]$fix$wordnum[j - 1]) {
      dat$trial[[trial]]$fix$word.refix[j] <- 1
    } else {
      dat$trial[[trial]]$fix$word.refix[j] <- 0
    }
    
    # IA
    if(dat$trial[[trial]]$fix$ianum[j] == dat$trial[[trial]]$fix$ianum[j - 1]) {
      dat$trial[[trial]]$fix$ia.refix[j] <- 1
    } else {
      dat$trial[[trial]]$fix$ia.refix[j] <- 0
    }
    
    # sent
    if(dat$trial[[trial]]$fix$sentnum[j] == dat$trial[[trial]]$fix$sentnum[j - 1]) {
      dat$trial[[trial]]$fix$sent.refix[j] <- 1
    } else {
      dat$trial[[trial]]$fix$sent.refix[j] <- 0
    }
    
  }
  
  return(dat)
  
}  