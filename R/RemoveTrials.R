
RemoveTrials <- function(dat, env = parent.frame(n = 2)){
  
  if (env$exp$setup$tracker$software == "EB") {
    
    practice <- env$exp$setup$clean$practice
    keep <- unlist(env$exp$setup$stimulus$file[env$exp$setup$stimulus$id])
    
    # msg
    dat$msg <- dat$msg[dat$msg$trialnum > practice, ]
    dat$msg$trialnum <- dat$msg$trialnum - practice
    dat$msg <- dat$msg[dat$msg$itemid %in% keep, ]
    
    # trial
    env$header$trial <- env$header$trial[env$header$trial$trialnum > practice, ]
    env$header$trial$trialnum <- env$header$trial$trialnum - practice
    env$header$trial <- env$header$trial[env$header$trial$itemid %in% keep, ]
    
  } else if (env$exp$setup$tracker$software == "ET") {
    
    practice <- env$exp$setup$clean$practice
    keep <- unlist(env$exp$setup$stimulus$file[env$exp$setup$stimulus$id])
    
    
    # msg
    # ----
    
    # remove practice trials
    dat$msg <- dat$msg[(dat$msg$itemid %in% practice) == F, ]
    
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
    dat$msg <- dat$msg[dat$msg$itemid %in% keep, ]
     
     
    # trial
    # ------
    
    # remove practice trials
    env$header$trial <- env$header$trial[(env$header$trial$itemid %in% practice) == F, ]

    # compute itemid
    tmp <- strsplit(env$header$trial$itemid, "P|E|I|D")
    env$header$trial$itemid <- as.numeric(sapply(tmp, "[[", 3))
    
    # NOTE: keep "E" in experimental trials (?)
    
    # remove trigger trials
    env$header$trial <- env$header$trial[-grep(env$exp$setup$item$trigger, env$header$trial$itemid), ]
    
    # remove questions trials
    env$header$trial <- env$header$trial[env$header$trial$itemid < env$exp$setup$item$question, ]
    
    # remove repeated trials
    tmp <- env$header$trial$itemid[env$header$trial$msg == env$exp$setup$message$start]
    exc <- unlist(names(table(tmp)[table(tmp) > 1]))
    if (is.null(exc) == FALSE) {
      env$header$trial <- env$header$trial[(env$header$trial$itemid %in% exc) == FALSE, ]
    }
    
    # recompute trialnum
    env$header$trial$trialnum <- as.numeric(factor(env$header$trial$trialnum))
    
    # keep items in stimulus file
    env$header$trial <- env$header$trial[env$header$trial$itemid %in% keep, ]
    
  }
  
  return(dat)
  
}
