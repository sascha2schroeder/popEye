
ComputeSkip <- function(dat, trial) {
  
  # trial <- 2
  
  
  # firstskip
  # ----------
  
  # initialize
  dat$trial[[trial]]$fix$word.firstskip <- 0
  word.mem <- 0
  
  dat$trial[[trial]]$fix$ia.firstskip <- 0
  ia.mem <- 0
  
  dat$trial[[trial]]$fix$sent.firstskip <- 0
  sent.mem <- 0
  
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
    
    # sent
    if (dat$trial[[trial]]$fix$sentnum[j] < max(sent.mem) & 
        is.element(dat$trial[[trial]]$fix$sentnum[j], sent.mem) == F) {
      dat$trial[[trial]]$fix$sent.firstskip[j] <- 1 
    }
    
    ia.mem <- c(ia.mem, dat$trial[[trial]]$fix$ianum[j])
    word.mem <- c(word.mem, dat$trial[[trial]]$fix$wordnum[j])
    sent.mem <- c(sent.mem, dat$trial[[trial]]$fix$wordnum[j])
    # print(j)
  }  
  
  dat$trial[[trial]]$fix$word.firstskip[is.na(dat$trial[[trial]]$fix$line) == T] <- NA
  dat$trial[[trial]]$fix$ia.firstskip[is.na(dat$trial[[trial]]$fix$line) == T] <- NA
  dat$trial[[trial]]$fix$sent.firstskip[is.na(dat$trial[[trial]]$fix$line) == T] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  
  # # skipping
  # # --------
  # 
  # # word
  # dat$trial[[trial]]$fix$word.skip <- 0
  # dat$trial[[trial]]$fix$word.skip[is.element(dat$trial[[trial]]$fix$wordnum, dat$trial[[trial]]$meta$stimmat$wordnum) == F] <- 1
  # 
  # # IA
  # dat$trial[[trial]]$fix$ia.skip <- 0
  # dat$trial[[trial]]$fix$ia.skip[is.element(dat$trial[[trial]]$fix$ianum, dat$trial[[trial]]$meta$stimmat$ianum) == F] <- 1
  # 
  # # sentence
  # dat$trial[[trial]]$fix$sent.skip <- 0
  # dat$trial[[trial]]$fix$sent.skip[is.element(dat$trial[[trial]]$fix$sentnum, dat$trial[[trial]]$meta$stimmat$sentnum) == F] <- 1
  
  return(dat)
  
}
