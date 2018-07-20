
AggregateTrial <- function(exp, env = parent.frame(n = 1)) {

  # create outfile  
  trialtmp <- exp$out$fix
  trialtmp$id <- paste(trialtmp$subid, trialtmp$trialnum, sep = ":")
  trial <- trialtmp[duplicated(trialtmp$id) == F, ]
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond")
  trial <- trial[names]  
  trial <- trial[order(trial$id), ]
  
  # compute measures
  trial$blink <- as.numeric(tapply(trialtmp$blink, list(trialtmp$id), max))
  trial$nrun <- as.numeric(tapply(trialtmp$runid, list(trialtmp$id), max))
  trial$nfix <- as.numeric(tapply(trialtmp$fixid, list(trialtmp$id), length))
  
  # compute forward saccade length (in letters)
  trialtmp$sac <- (trialtmp$land + trialtmp$launch)
  trial$sac <- NA
  trial$sac <- round(as.numeric(tapply(trialtmp$sac[trialtmp$sac >= 0], list(trialtmp$id[trialtmp$sac >= 0]), mean)), 2)
  
  # # backward
  # backtmp <- aggregate(trialtmp$sac[trialtmp$sac < 0], list(trialtmp$id[trialtmp$sac < 0]), mean)
  # colnames(backtmp) <- c("id", "sac.back")
  # backtmp$sac.back <- round(abs(backtmp$sac.back), 2)
  # trial <- merge(trial, backtmp)
  
  # match with ia file 
  ia <- exp$out$ia
  ia$id <- paste(ia$subid, ia$trialnum, sep = ":")
  
  trial$skip <- round(as.numeric(tapply(ia$ia.firstskip, list(ia$id), mean)), 3)
  trial$refix <- round(as.numeric(tapply(ia$ia.refix, list(ia$id), mean, na.rm = T)), 3)
  # this is the proportion of words that have been refixated
  trial$reg <- round(as.numeric(tapply(ia$ia.reg.in, list(ia$id), mean, na.rm = T)), 3)
  # this is the proportion of words that have been regressed to
  
  # trial$refix.fix <- round(as.numeric(tapply(trialtmp$refix, list(trialtmp$trialnum), mean)), 3)
  # # this is the proportion of fixations that are refixations
  # trial$reg.fix <- round(as.numeric(tapply(trialtmp$reg.in, list(trialtmp$trialnum), mean)), 3)
  # # this is the proportion of fixations that are regressions
  
  # trial duration (only fixation time)
  trial$dur <- as.numeric(tapply(trialtmp$dur, list(trialtmp$id), sum))
  
  # reading rate
  trial$nwords <- tapply(trialtmp$word, list(trialtmp$id), max)
  # trial$nia <- tapply(ia$subid, list(ia$id), length)
  trial$rate <- round(60000 / (trial$dur / trial$nwords))
  # NOTE: This is only reading reate if IA is word
  
  # mean fixation duration
  fix <- exp$out$fix
  fix$id <- paste(fix$subid, fix$trialnum, sep = ":")
  trial$mfix <- round(as.numeric(tapply(fix$dur, list(fix$id), mean)))
  
  # return
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "nwords", 
             "blink", "nrun", "nfix", "sac", "skip", "refix", "reg", "mfix", 
             "dur", "rate")
  exp$out$trial <- trial[order(trial$subid, trial$trialnum), names]
  trial$id <- NULL
  row.names(exp$out$trial) <- NULL
  
  return(exp)
  
}

