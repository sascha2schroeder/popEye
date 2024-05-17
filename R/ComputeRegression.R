
ComputeRegression <- function(dat, trial) {
  
  # initialize
  
  tmp <- dat$item[[trial]]$fix[dat$item[[trial]]$fix$type == "in", ]
  
  tmp$word.reg.out <- 0
  tmp$word.reg.in <- 0
  tmp$word.reg.out.to <- NA
  tmp$word.reg.in.from <- NA
  
  tmp$ia.reg.out <- 0
  tmp$ia.reg.in <- 0
  tmp$ia.reg.out.to <- NA
  tmp$ia.reg.in.from <- NA
  
  tmp$sent.reg.out <- 0
  tmp$sent.reg.in <- 0
  tmp$sent.reg.out.to <- NA
  tmp$sent.reg.in.from <- NA
  
  if (nrow(tmp) > 1) {
    for (j in 2:nrow(tmp)){
      # j <- 2
      
      # skip outliers
      if(is.na(tmp$wordnum[j]) | is.na(tmp$wordnum[j - 1])) next
      # NOTE: delete if outliers are excluded earlier
      
      # word
      if(tmp$wordnum[j] < tmp$wordnum[j - 1]) {
        tmp$word.reg.in[j] <- 1
        tmp$word.reg.out[j - 1] <- 1
        tmp$word.reg.in.from [j] <- tmp$wordnum[j - 1]
        tmp$word.reg.out.to[j - 1] <- tmp$wordnum[j]
      }
      
      # IA
      if(tmp$ianum[j] < tmp$ianum[j - 1]) {
        tmp$ia.reg.in[j] <- 1
        tmp$ia.reg.out[j - 1] <- 1
        tmp$ia.reg.in.from[j] <- tmp$ianum[j - 1]
        tmp$ia.reg.out.to[j - 1] <- tmp$ianum[j]
      }
      
      # sent
      if(tmp$sentnum[j] < tmp$sentnum[j - 1]) {
        tmp$sent.reg.in[j] <- 1
        tmp$sent.reg.out[j - 1] <- 1
        tmp$sent.reg.in.from[j] <- tmp$sentnum[j - 1]
        tmp$sent.reg.out.to[j - 1] <- tmp$sentnum[j]
      }
      
    }
    
  }
  
  names <- c("num", 
             "word.reg.out", "word.reg.in", "word.reg.out.to", "word.reg.in.from", 
             "ia.reg.out", "ia.reg.in", "ia.reg.out.to", "ia.reg.in.from",
             "sent.reg.out", "sent.reg.in", "sent.reg.out.to", "sent.reg.in.from")
  tmp <- tmp[names]
  dat$item[[trial]]$fix <- merge(dat$item[[trial]]$fix, tmp, by = "num", all.x = T)
  
  return(dat)
  
}
