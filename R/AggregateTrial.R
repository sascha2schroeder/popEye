
AggregateTrial <- function(exp, env = parent.frame(n = 1)) {

  # create outfile  
  trialtmp <- exp$out$fix
  
  trialtmp$id <- paste(trialtmp$subid, trialtmp$trialnum, sep = ":")
  trial <- trialtmp[duplicated(trialtmp$id) == F, ]
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "trial", 
             "trial.nwords")
  trial <- trial[names]  
  trial <- trial[order(trial$id), ]
  
  
  # compute measures
  # -----------------
  
  # number of blinks
  blink <- aggregate(trialtmp$blink, list(trialtmp$id), function(x) round(sum(x) / 2))
  colnames(blink) <- c("id", "nblink")
  trial <- merge(trial, blink, all.x = T)

  # number of runs
  trial$nrun <- as.numeric(tapply(trialtmp$word.runid, list(trialtmp$id), max, na.rm = T))
  
  # number of fixations
  trial$nfix <- as.numeric(tapply(trialtmp$fixid[trialtmp$type == "in"], list(trialtmp$id[trialtmp$type == "in"]), length))

  # number of outliers  
  nout <- aggregate(trialtmp$type == "out", list(trialtmp$id), sum)
  colnames(nout) <- c("id", "nout")
  trial <- merge(trial, nout, all.x = T)
  trial$pout <- round(trial$nout / (trial$nfix + trial$nout), 3)
  
  # compute forward saccade length (in letters)
  trialtmp$sac <- (trialtmp$word.land + trialtmp$word.launch)
  sac <- aggregate(trialtmp$sac[trialtmp$sac >= 0 & is.na(trialtmp$sac) == F], list(trialtmp$id[trialtmp$sac >= 0 & is.na(trialtmp$sac) == F]), mean, na.rm = T)
  colnames(sac) <- c("id", "sac")
  sac$sac <- round(sac$sac, 2)
  trial <- merge(trial, sac, all.x = T)
  
  # mean fixation duration
  trial$mfix <- round(as.numeric(tapply(trialtmp$dur, list(trialtmp$id), mean)))
  
  # trial duration
  trial$total <- as.numeric(tapply(trialtmp$dur[trialtmp$type == "in"], list(trialtmp$id[trialtmp$type == "in"]), sum))
  # NOTE: only fixation time, does not include saccades and outliers
  # NOTE: maybe differentiate between "trial time" and "reading time"
  
  
  # reading rate
  trial$rate <- round(60000 / (trial$total / trial$trial.nwords))
  
  # match with word-level file 
  word <- exp$out$word
  word$id <- paste(word$subid, word$trialnum, sep = ":")
  
  trial$skip <- round(as.numeric(tapply(word$firstrun.skip, list(word$id), mean, na.rm = T)), 3)
  trial$refix <- round(as.numeric(tapply(word$refix, list(word$id), mean, na.rm = T)), 3)
  # this is the proportion of words that have been refixated
  trial$reg <- round(as.numeric(tapply(word$reg.in, list(word$id), mean, na.rm = T)), 3)
  # this is the proportion of words that have been regressed to

  # compute first-pass reading time
  trial$firstpass <- round(as.numeric(tapply(word$firstrun.dur, list(word$id), sum, na.rm = T)))
  
  # compute rereading time
  trial$rereading <- round(as.numeric(tapply(word$dur - word$firstrun.dur, list(word$id), sum, na.rm = T)))
  
  # return
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "trial", "trial.nwords", 
             "nblink", "nrun", "nfix", "nout", "pout", "sac", "skip", "refix", 
             "reg", "mfix", "firstpass", "rereading", "total", "rate")
  exp$out$trial <- trial[order(trial$subid, trial$trialnum), names]
  trial$id <- NULL
  row.names(exp$out$trial) <- NULL
  
  return(exp)
  
}

