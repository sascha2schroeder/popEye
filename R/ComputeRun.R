
ComputeRun <- function(dat, trial) {
  # trial = 1
  
  tmp <- dat$trial[[trial]]$fix[dat$trial[[trial]]$fix$type == "in", ]
  
  # run
  # ----
  
  # initialize
  tmp$word.runid <- 1
  tmp$ia.runid <- 1
  
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
      # print(j)
    }
  }
  
  # fixid
  # ------
  
  # fixid in word
  tmp$word.fix = ave(tmp$num, tmp$wordnum, FUN = rank)
  
  # fixid in IA
  tmp$ia.fix = ave(tmp$num, tmp$ianum, FUN = rank)
  
  
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
  
  # outdat <- 
  names <- c("num", "word.runid", "ia.runid", "word.fix", "ia.fix",
             "word.run", "ia.run", "word.run.fix", "ia.run.fix")
  tmp <- tmp[names]
  
  dat$trial[[trial]]$fix <- merge(dat$trial[[trial]]$fix, tmp, by = "num", all.x = T)
  
  return(dat)
  
}