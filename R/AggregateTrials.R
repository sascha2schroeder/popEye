
AggregateTrials <- function(exp, env = parent.frame(n = 1)) {

  # create outfile  
  trialtmp <- exp$out$fix
  trialtmp$id <- paste(trialtmp$subid, trialtmp$trialnum, sep = ":")
  
  tmp <- trialtmp[is.na(trialtmp$trial.nwords) == F, ]
  trial <- tmp[duplicated(tmp$id) == F, ]
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "trial", 
             "trial.nwords")
  trial <- trial[names]  
  trial <- trial[order(trial$id), ]
  
  
  # compute measures
  # -----------------
  
  # number of blinks
  blink <- aggregate(tmp$blink[tmp$type == "in"], list(tmp$id[tmp$type == "in"]), function(x) round(sum(x) / 2))
  colnames(blink) <- c("id", "nblink")
  trial <- merge(trial, blink, all.x = T)

  # number of runs
  trial$nrun <- as.numeric(tapply(tmp$word.runid[tmp$type == "in"], list(tmp$id[tmp$type == "in"]), max, na.rm = T))
  
  # number of fixations
  trial$nfix <- as.numeric(tapply(tmp$fixid[tmp$type == "in"], list(tmp$id[tmp$type == "in"]), length))

  # number of outliers  
  nout <- aggregate(trialtmp$type == "out", list(trialtmp$id), sum)
  colnames(nout) <- c("id", "nout")
  trial <- merge(trial, nout, all.x = T)
  
  # compute forward saccade length (in letters)
  tmp$sac <- (tmp$word.land + tmp$word.launch)
  sac <- aggregate(tmp$sac[tmp$sac >= 0 & is.na(tmp$sac) == F], list(tmp$id[tmp$sac >= 0 & is.na(tmp$sac) == F]), mean, na.rm = T)
  colnames(sac) <- c("id", "sac")
  sac$sac <- round(sac$sac, 3)
  trial <- merge(trial, sac, all.x = T)
  
  # mean fixation duration
  trial$mfix <- round(as.numeric(tapply(tmp$dur[tmp$type == "in"], list(tmp$id[tmp$type == "in"]), mean)))
  
  # trial duration
  trial$total <- as.numeric(tapply(tmp$dur[tmp$type == "in"], list(tmp$id[tmp$type == "in"]), sum))
  # NOTE: only fixation time, does not include saccades and outliers
  # NOTE: maybe differentiate between "trial time" and "reading time"
  
  # reading rate
  trial$rate <- round(60000 / (trial$total / trial$trial.nwords))
  
  # match with word-level file 
  word <- exp$out$words
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
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "trial", 
             "trial.nwords", "nblink", "nrun", "nfix", "nout", 
             "sac", "skip", "refix", "reg", "mfix", "firstpass", 
             "rereading", "total", "rate")
  
  exp$out$trials <- trial[order(trial$subid, trial$trialnum), names]
  # trial$id <- NULL
  row.names(exp$out$trials) <- NULL
  
  return(exp)
  
}

