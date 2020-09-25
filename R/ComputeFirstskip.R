
ComputeFirstskip <- function(dat, trial) {
  
  # trial <- 2
  
  # initialize
  dat$item[[trial]]$fix$word.firstskip <- 0
  word.mem <- 0
  
  dat$item[[trial]]$fix$ia.firstskip <- 0
  ia.mem <- 0
  
  dat$item[[trial]]$fix$sent.firstskip <- 0
  sent.mem <- 0
  
  # fixation loop
  for (j in 1:nrow(dat$item[[trial]]$fix)){
    # j <- 1
    
    # skip outliers
    if(dat$item[[trial]]$fix$type[j] == "out") next
    # NOTE: delete if outliers are excluded earlier
    
    # word
    if (dat$item[[trial]]$fix$wordnum[j] < max(word.mem) & 
        is.element(dat$item[[trial]]$fix$wordnum[j], word.mem) == F) {
      dat$item[[trial]]$fix$word.firstskip[j] <- 1 
    }

    # IA
    if (dat$item[[trial]]$fix$ianum[j] < max(ia.mem) & 
        is.element(dat$item[[trial]]$fix$ianum[j], ia.mem) == F) {
     dat$item[[trial]]$fix$ia.firstskip[j] <- 1 
    }
    
    # sent
    if (dat$item[[trial]]$fix$sentnum[j] < max(sent.mem) & 
        is.element(dat$item[[trial]]$fix$sentnum[j], sent.mem) == F) {
      dat$item[[trial]]$fix$sent.firstskip[j] <- 1 
    }
    
    word.mem <- c(word.mem, dat$item[[trial]]$fix$wordnum[j])
    ia.mem <- c(ia.mem, dat$item[[trial]]$fix$ianum[j])
    sent.mem <- c(sent.mem, dat$item[[trial]]$fix$sentnum[j])
    # print(j)
  }  
  
  dat$item[[trial]]$fix$word.firstskip[is.na(dat$item[[trial]]$fix$line) == T] <- NA
  dat$item[[trial]]$fix$ia.firstskip[is.na(dat$item[[trial]]$fix$line) == T] <- NA
  dat$item[[trial]]$fix$sent.firstskip[is.na(dat$item[[trial]]$fix$line) == T] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  return(dat)
  
}
