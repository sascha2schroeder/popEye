
AggregateTrial <- function(exp) {

  # create outfile  
  trialtmp <- exp$out$fix
  trialtmp$id <- paste(trialtmp$subid, trialtmp$trialnum, sep = ":")
  trial <- trialtmp[duplicated(trialtmp$id) == F, ]
  names <- c("id", "subid", "trialnum", "itemid", "cond")
  trial <- trial[names]  
  trial <- trial[order(trial$id), ]
  
  # compute measures
  trial$blink <- as.numeric(tapply(trialtmp$blink, list(trialtmp$id), max))
  trial$nrun <- as.numeric(tapply(trialtmp$runid, list(trialtmp$id), max))
  trial$nfix <- as.numeric(tapply(trialtmp$fixid, list(trialtmp$id), length))
  
  # match with ia file 
  ia <- exp$out$ia
  ia$id <- paste(ia$subid, ia$trialnum, sep = ":")
  
  trial$skip <- round(as.numeric(tapply(ia$ia.firstskip, list(ia$id), mean)), 3)
  trial$refix <- round(as.numeric(tapply(ia$ia.refix, list(ia$id), mean, na.rm = T)), 3)
  # this is the proportion of words that have been refixated
  trial$reg <- round(as.numeric(tapply(ia$ia.reg, list(ia$id), mean, na.rm = T)), 3)
  # this is the proportion of words that have been regressed to
  
  # trial$refix.fix <- round(as.numeric(tapply(trialtmp$refix, list(trialtmp$trialnum), mean)), 3)
  # # this is the proportion of fixations that are refixations
  # trial$reg.fix <- round(as.numeric(tapply(trialtmp$reg.in, list(trialtmp$trialnum), mean)), 3)
  # # this is the proportion of fixations that are regressions
  
  trial$dur <- as.numeric(tapply(trialtmp$dur, list(trialtmp$id), sum))
  
  trial$id <- NULL
  
  # save
  exp$out$trial <- trial[order(trial$subid, trial$trialnum), ]
  row.names(exp$out$trial) <- NULL
  
  return(exp)
  
}

