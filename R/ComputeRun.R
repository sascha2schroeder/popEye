
ComputeRun <- function(dat, trial) {
  # trial = 1
  
  
  # run
  # ----
  
  # initialize
  dat$trial[[trial]]$fix$ia.runid <- 1
  dat$trial[[trial]]$fix$word.runid <- 1
  
  # fixation loop
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    # j <- 2
    
    # skip outliers
    if(is.na(dat$trial[[trial]]$fix$wordnum[j]) | is.na(dat$trial[[trial]]$fix$wordnum[j - 1])) next
    # NOTE: delete if outliers are excluded earlier
    
    # word
    if (dat$trial[[trial]]$fix$word.reg.in[j] == 1 & dat$trial[[trial]]$fix$word.reg.in[j - 1] != 1) {
      dat$trial[[trial]]$fix$word.runid[j] <- dat$trial[[trial]]$fix$word.runid[j - 1] + 1
    } else {
      dat$trial[[trial]]$fix$word.runid[j] <- dat$trial[[trial]]$fix$word.runid[j - 1]
    }
    
    # IA
    if (dat$trial[[trial]]$fix$ia.reg.in[j] == 1 & dat$trial[[trial]]$fix$ia.reg.in[j - 1] != 1) {
      dat$trial[[trial]]$fix$ia.runid[j] <- dat$trial[[trial]]$fix$ia.runid[j - 1] + 1
    } else {
      dat$trial[[trial]]$fix$ia.runid[j] <- dat$trial[[trial]]$fix$ia.runid[j - 1]
    }
  }
  
  dat$trial[[trial]]$fix$word.runid[dat$trial[[trial]]$fix$line == 0] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  dat$trial[[trial]]$fix$ia.runid[dat$trial[[trial]]$fix$line == 0] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  
  # fixid
  # ------
  
  # fixid in word
  dat$trial[[trial]]$fix$word.fix = ave(dat$trial[[trial]]$fix$num, 
                                        dat$trial[[trial]]$fix$wordnum, FUN = rank)
  
  dat$trial[[trial]]$fix$word.fix[dat$trial[[trial]]$fix$line == 0] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  # fixid in IA
  dat$trial[[trial]]$fix$ia.fix = ave(dat$trial[[trial]]$fix$num, 
                                      dat$trial[[trial]]$fix$ianum, FUN = rank)
  
  dat$trial[[trial]]$fix$ia.fix[dat$trial[[trial]]$fix$line == 0] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  
  # runid
  # ------
  
  # runid in word
  dat$trial[[trial]]$fix$id <- 
    paste(dat$trial[[trial]]$fix$wordnum, dat$trial[[trial]]$fix$word.runid, sep = ":")
  fix.tmp <- dat$trial[[trial]]$fix[duplicated(dat$trial[[trial]]$fix$id) == F, ]
  fix.tmp$word.run <- ave(fix.tmp$word.runid, fix.tmp$wordnum, FUN = rank)
  dat$trial[[trial]]$fix <- merge(dat$trial[[trial]]$fix, 
                                  fix.tmp[c("id", "word.run")], by = "id")
  dat$trial[[trial]]$fix$id <- NULL
  dat$trial[[trial]]$fix <- dat$trial[[trial]]$fix[order(dat$trial[[trial]]$fix$num), ]
  
  dat$trial[[trial]]$fix$word.run[dat$trial[[trial]]$fix$line == 0] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  # runid in IA
  dat$trial[[trial]]$fix$id <- 
    paste(dat$trial[[trial]]$fix$ianum, dat$trial[[trial]]$fix$ia.runid, sep = ":")
  fix.tmp <- dat$trial[[trial]]$fix[duplicated(dat$trial[[trial]]$fix$id) == F, ]
  fix.tmp$ia.run <- ave(fix.tmp$ia.runid, fix.tmp$ianum, FUN = rank)
  dat$trial[[trial]]$fix <- merge(dat$trial[[trial]]$fix, 
                                  fix.tmp[c("id", "ia.run")], by = "id")
  dat$trial[[trial]]$fix$id <- NULL
  dat$trial[[trial]]$fix <- dat$trial[[trial]]$fix[order(dat$trial[[trial]]$fix$num), ]
 
  dat$trial[[trial]]$fix$ia.run[dat$trial[[trial]]$fix$line == 0] <- NA
  # NOTE: delete if outliers are excluded earlier
  
   
  # fixnum
  # -------
  
  # fixnum in word.run
  dat$trial[[trial]]$fix$id <- 
    paste(dat$trial[[trial]]$fix$wordnum, dat$trial[[trial]]$fix$word.run, sep = ":")
  dat$trial[[trial]]$fix$word.run.fix <- 
    ave(dat$trial[[trial]]$fix$num, dat$trial[[trial]]$fix$id, FUN = rank)
  dat$trial[[trial]]$fix$id <- NULL
  dat$trial[[trial]]$fix <- dat$trial[[trial]]$fix[order(dat$trial[[trial]]$fix$num), ]
  
  dat$trial[[trial]]$fix$word.run.fix[dat$trial[[trial]]$fix$line == 0] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  # fixnum in ia.run
  dat$trial[[trial]]$fix$id <- 
    paste(dat$trial[[trial]]$fix$ianum, dat$trial[[trial]]$fix$ia.run, sep = ":")
  dat$trial[[trial]]$fix$ia.run.fix <- 
    ave(dat$trial[[trial]]$fix$num, dat$trial[[trial]]$fix$id, FUN = rank)
  dat$trial[[trial]]$fix$id <- NULL
  dat$trial[[trial]]$fix <- dat$trial[[trial]]$fix[order(dat$trial[[trial]]$fix$num), ]
  
  dat$trial[[trial]]$fix$ia.run.fix[dat$trial[[trial]]$fix$line == 0] <- NA
  # NOTE: delete if outliers are excluded earlier
  
  return(dat)
  
}
