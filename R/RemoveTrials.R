
RemoveTrials <- function(dat, env = parent.frame(n = 2)){
  
  if (env$exp$setup$tracker$software == "EB") {
    
    # remove practice trials
    dat$msg <- dat$msg[dat$msg$trialnum > env$exp$setup$clean$practice, ]
    
    # recompute trialnum
    dat$msg$trialnum <- dat$msg$trialnum - env$exp$setup$clean$practice
    
    # exclude items
    env$exp$setup$item$keep <- unlist(env$exp$setup$stimulus$file[env$exp$setup$stimulus$id])
    # NOTE: keep all items in stimulusfile

    # if (env$exp$setup$item$keep == "") {
    #   env$exp$setup$item$keep <- unlist(dimnames(table(dat$msg$itemid)))
    # } 

    dat$msg <- dat$msg[dat$msg$itemid %in% env$exp$setup$item$keep, ]

    # env$exp$setup$item$keep <- ""
    
    # exclude corresponding sample and event data
    dat$samp <- dat$samp[dat$samp$time > (dat$msg$time[1] - 1), ]
    dat$event <- dat$event[dat$event$time > (dat$msg$time[1] - 1), ]
    
  } else if (env$exp$setup$tracker$software == "ET") {
    
    dat$msg <- dat$msg[(dat$msg$itemid %in% env$exp$setup$clean$practice) == F, ]

    # remove practice trials
    if (sum(dat$msg$itemid == env$exp$setup$clean$practice) > 1) { # FIX: check
      # dat$msg <- dat$msg[dat$msg$trialnum > dat$msg$trialnum[
      #   dat$msg$itemid == env$exp$setup$clean$practice][1], ]
      dat$msg <- dat$msg[(dat$msg$itemid %in% env$exp$setup$clean$practice) == F, ]
      
    } else {
      # dat$msg <- dat$msg[dat$msg$trialnum > dat$msg$trialnum[
      #   dat$msg$itemid == env$exp$setup$clean$practice - 1][1], ]
      dat$msg <- dat$msg[(dat$msg$itemid %in% env$exp$setup$clean$practice) == F, ]
    }

    # remove letters from itemid
    tmp <- strsplit(dat$msg$itemid, "P|E|I|D")
    
    # itemid
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
    
    # exclude items
    if (env$exp$setup$item$keep == "") {
      keep <- unlist(dimnames(table(dat$msg$itemid)))
    } else {
      keep <- env$exp$setup$item$keep
    }
    dat$msg <- dat$msg[dat$msg$itemid %in% keep, ]
    # NOTE: all items as default
    
    # exclude corresponding samples and events
    dat$samp <- dat$samp[dat$samp$time > (dat$msg$time[1] - 1), ]
    dat$event <- dat$event[dat$event$time > (dat$msg$time[1] - 1), ]
    
  }
  
  return(dat)
  
}
