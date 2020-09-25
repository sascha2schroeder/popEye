
ComputeRun <- function(dat, trial) {
  # trial = 1
  
  tmp <- dat$item[[trial]]$fix[dat$item[[trial]]$fix$type == "in", ]
  
  # run
  # ----
  
  # initialize
  tmp$word.runid <- 1
  tmp$ia.runid <- 1
  tmp$sent.runid <- 1
  
  # fixation loop
  if (nrow(tmp) > 1) {
    
    for (j in 2:nrow(tmp)){
      # j <- 2
      
      # word
      if (tmp$word.reg.in[j] == 1 & tmp$word.reg.in[j - 1] != 1) {
        tmp$word.runid[j] <- tmp$word.runid[j - 1] + 1
      } else {
        tmp$word.runid[j] <- tmp$word.runid[j - 1]
      }
      
      # IA
      if (tmp$ia.reg.in[j] == 1 & tmp$ia.reg.in[j - 1] != 1) {
        tmp$ia.runid[j] <- tmp$ia.runid[j - 1] + 1
      } else {
        tmp$ia.runid[j] <- tmp$ia.runid[j - 1]
      }
      
      # sent
      if (tmp$sent.reg.in[j] == 1 & tmp$sent.reg.in[j - 1] != 1) {
        tmp$sent.runid[j] <- tmp$sent.runid[j - 1] + 1
      } else {
        tmp$sent.runid[j] <- tmp$sent.runid[j - 1]
      }
      
      # print(j)
    }
  }
  
  # fixid
  # ------
  
  # fixid in word
  tmp$word.fix = ave(tmp$num, tmp$wordnum, FUN = rank)
  
  # fixid in IA
  tmp$ia.fix = ave(tmp$num, tmp$ianum, FUN = rank)
  
  # fixid in sent
  tmp$sent.fix = ave(tmp$num, tmp$sentnum, FUN = rank)
  
  # runid
  # ------
  
  # runid in word
  tmp$id <- paste(tmp$wordnum, tmp$word.runid, sep = ":")
  fix.tmp <- tmp[duplicated(tmp$id) == F, ]
  fix.tmp$word.run <- ave(fix.tmp$word.runid, fix.tmp$wordnum, FUN = rank)
  tmp <- merge(tmp, fix.tmp[c("id", "word.run")], by = "id")
  tmp$id <- NULL
  tmp <- tmp[order(tmp$num), ]
  
  # runid in IA
  tmp$id <- paste(tmp$ianum, tmp$ia.runid, sep = ":")
  fix.tmp <- tmp[duplicated(tmp$id) == F, ]
  fix.tmp$ia.run <- ave(fix.tmp$ia.runid, fix.tmp$ianum, FUN = rank)
  tmp <- merge(tmp, fix.tmp[c("id", "ia.run")], by = "id")
  tmp$id <- NULL
  tmp <- tmp[order(tmp$num), ]

  # runid in sentence
  tmp$id <- paste(tmp$sentnum, tmp$sent.runid, sep = ":")
  fix.tmp <- tmp[duplicated(tmp$id) == F, ]
  fix.tmp$sent.run <- ave(fix.tmp$sent.runid, fix.tmp$sentnum, FUN = rank)
  tmp <- merge(tmp, fix.tmp[c("id", "sent.run")], by = "id")
  tmp$id <- NULL
  tmp <- tmp[order(tmp$num), ]
  
  
  # fixnum
  # -------
  
  # fixnum in word.run
  tmp$id <- paste(tmp$wordnum, tmp$word.run, sep = ":")
  tmp$word.run.fix <- ave(tmp$num, tmp$id, FUN = rank)
  tmp$id <- NULL
  tmp <- tmp[order(tmp$num), ]
  
  # fixnum in ia.run
  tmp$id <- paste(tmp$ianum, tmp$ia.run, sep = ":")
  tmp$ia.run.fix <- ave(tmp$num, tmp$id, FUN = rank)
  tmp$id <- NULL
  tmp <- tmp[order(tmp$num), ]
  
  # fixnum in ia.run
  tmp$id <- paste(tmp$sentnum, tmp$sent.run, sep = ":")
  tmp$sent.run.fix <- ave(tmp$num, tmp$id, FUN = rank)
  tmp$id <- NULL
  tmp <- tmp[order(tmp$num), ]
  
  # outdat <- 
  names <- c("num", "word.runid", "ia.runid", "sent.runid", 
             "word.fix", "ia.fix", "sent.fix",
             "word.run", "ia.run", "sent.run",
             "word.run.fix", "ia.run.fix", "sent.run.fix")
  tmp <- tmp[names]
  
  dat$item[[trial]]$fix <- merge(dat$item[[trial]]$fix, tmp, by = "num", all.x = T)
  
  return(dat)
  
}
