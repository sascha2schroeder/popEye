
ComputeRegression <- function(dat, trial) {
  
  # initialize
  
  tmp <- dat$trial[[trial]]$fix[dat$trial[[trial]]$fix$type == "in", ]
  
  tmp$word.reg.out <- 0
  tmp$word.reg.in <- 0
  
  tmp$ia.reg.out <- 0
  tmp$ia.reg.in <- 0
  
  for (j in 2:nrow(tmp)){
    # j <- 2
    
    # skip outliers
    if(is.na(tmp$wordnum[j]) | is.na(tmp$wordnum[j - 1])) next
    # NOTE: delete if outliers are excluded earlier
    
    # word
    if(tmp$wordnum[j] < tmp$wordnum[j - 1]) {
      tmp$word.reg.in[j] <- 1
      tmp$word.reg.out[j - 1] <- 1
    }
    
    # IA
    if(tmp$ianum[j] < tmp$ianum[j - 1]) {
      tmp$ia.reg.in[j] <- 1
      tmp$ia.reg.out[j - 1] <- 1
    }
    
  }
  
  names <- c("num", "word.reg.out", "word.reg.in", "ia.reg.out", "ia.reg.in")
  tmp <- tmp[names]
  dat$trial[[trial]]$fix <- merge(dat$trial[[trial]]$fix, tmp, by = "num", all.x = T)
  
  return(dat)
  
}
