
ComputeFirstskip <- function(dat, trial) {
  
  # trial <- 2
  
  # initialize
  dat$trial[[trial]]$fix$word.firstskip <- 0
  word.mem <- 0
  
  dat$trial[[trial]]$fix$ia.firstskip <- 0
  ia.mem <- 0
  
  # fixation loop
  for (j in 1:nrow(dat$trial[[trial]]$fix)){
    # j <- 1

    # skip outliers
    if(dat$trial[[trial]]$fix$type[j] == "out") next
    # NOTE: delete if outliers are excluded earlier
    
    # word
    if (dat$trial[[trial]]$fix$wordnum[j] < max(word.mem) & 
        is.element(dat$trial[[trial]]$fix$wordnum[j], word.mem) == F) {
      dat$trial[[trial]]$fix$word.firstskip[j] <- 1 
    }

    # IA
    if (dat$trial[[trial]]$fix$ianum[j] < max(ia.mem) & 
        is.element(dat$trial[[trial]]$fix$ianum[j], ia.mem) == F) {
     dat$trial[[trial]]$fix$ia.firstskip[j] <- 1 
    }
    
    ia.mem <- c(ia.mem, dat$trial[[trial]]$fix$ianum[j])
    word.mem <- c(word.mem, dat$trial[[trial]]$fix$wordnum[j])
    # print(j)
  }  
  
  dat$trial[[trial]]$fix$word.firstskip[is.na(dat$trial[[trial]]$fix$line) == T] <- NA
  dat$trial[[trial]]$fix$ia.firstskip[is.na(dat$trial[[trial]]$fix$line) == T] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  return(dat)
  
}
