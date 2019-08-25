
RemoveTrials <- function(dat, env = parent.frame(n = 2)){
  
  if (env$exp$setup$tracker$software == "EB") {
    
    # remove practice trials
    dat$msg <- dat$msg[dat$msg$trialnum > env$exp$setup$clean$practice, ]
    
    # recompute trialnum
    dat$msg$trialnum <- dat$msg$trialnum - env$exp$setup$clean$practice
    
    # keep only item in stimulus file
    keep <- unlist(env$exp$setup$stimulus$file[env$exp$setup$stimulus$id])
    dat$msg <- dat$msg[dat$msg$itemid %in% keep, ]

  } else if (env$exp$setup$tracker$software == "ET") {
    
    # remove practice trials
    dat$msg <- dat$msg[(dat$msg$itemid %in% env$exp$setup$clean$practice) == F, ]
    
    # compute itemid
    tmp <- strsplit(dat$msg$itemid, "P|E|I|D")
    dat$msg$itemid <- as.numeric(sapply(tmp, "[[", 3))
    
    # remove trigger trials
    dat$msg <- dat$msg[-grep(env$exp$setup$item$trigger, dat$msg$itemid), ]
    
    # remove questions trials
    dat$msg <- dat$msg[dat$msg$itemid < env$exp$setup$item$question, ]
    
    # remove repeated trials
    tmp <- dat$msg$itemid[dat$msg$msg == env$exp$setup$message$start]
    exc <- unlist(names(table(tmp)[table(tmp) > 1]))
    if (is.null(exc) == FALSE) {
      dat$msg <- dat$msg[(dat$msg$itemid %in% exc) == FALSE, ]
    }
    
    # recompute trialnum
    dat$msg$trialnum <- as.numeric(factor(dat$msg$trialnum))
    
    # keep items in stimulus file
    keep <- unlist(env$exp$setup$stimulus$file[env$exp$setup$stimulus$id])
    dat$msg <- dat$msg[dat$msg$itemid %in% keep, ]
      
  }
  
  return(dat)
  
}
