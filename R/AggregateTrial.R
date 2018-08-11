
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
  trial$nrun <- as.numeric(tapply(trialtmp$word.runid, list(trialtmp$id), max))
  trial$nfix <- as.numeric(tapply(trialtmp$fixid, list(trialtmp$id), length))
  
  # compute forward saccade length (in letters)
  trialtmp$sac <- (trialtmp$word.land + trialtmp$word.launch)
  trial$sac <- NA
  trial$sac <- round(as.numeric(tapply(trialtmp$sac[trialtmp$sac >= 0], list(trialtmp$id[trialtmp$sac >= 0]), mean)), 2)
  
  # # backward
  # backtmp <- aggregate(trialtmp$sac[trialtmp$sac < 0], list(trialtmp$id[trialtmp$sac < 0]), mean)
  # colnames(backtmp) <- c("id", "sac.back")
  # backtmp$sac.back <- round(abs(backtmp$sac.back), 2)
  # trial <- merge(trial, backtmp)
  
  # mean fixation duration
  trial$mfix <- round(as.numeric(tapply(trialtmp$dur, list(trialtmp$id), mean)))
  
  # trial$refix.fix <- round(as.numeric(tapply(trialtmp$refix, list(trialtmp$trialnum), mean)), 3)
  # # this is the proportion of fixations that are refixations
  # trial$reg.fix <- round(as.numeric(tapply(trialtmp$reg.in, list(trialtmp$trialnum), mean)), 3)
  # # this is the proportion of fixations that are regressions
  
  # trial duration (only fixation time)
  trial$total <- as.numeric(tapply(trialtmp$dur, list(trialtmp$id), sum))
  
  # reading rate
  trial$nwords <- tapply(trialtmp$wordnum, list(trialtmp$id), max)
  trial$rate <- round(60000 / (trial$total / trial$nwords))
  
  # match with word-level file 
  word <- exp$out$word
  word$id <- paste(word$subid, word$trialnum, sep = ":")
  
  trial$skip <- round(as.numeric(tapply(word$firstrun.skip, list(word$id), mean)), 3)
  trial$refix <- round(as.numeric(tapply(word$refix, list(word$id), mean, na.rm = T)), 3)
  # this is the proportion of words that have been refixated
  trial$reg <- round(as.numeric(tapply(word$reg.in, list(word$id), mean, na.rm = T)), 3)
  # this is the proportion of words that have been regressed to

  # compute first-pass reading time
  trial$firstpass <- round(as.numeric(tapply(word$firstrun.dur, list(word$id), sum, na.rm = T)))
  
  # compute rereading time
  trial$rereading <- round(as.numeric(tapply(word$dur - word$firstrun.dur, list(word$id), sum, na.rm = T)))
  
  # return
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "nwords", 
             "blink", "nrun", "nfix", "sac", "skip", "refix", "reg", "mfix", 
             "firstpass", "rereading", "total", "rate")
  exp$out$trial <- trial[order(trial$subid, trial$trialnum), names]
  trial$id <- NULL
  row.names(exp$out$trial) <- NULL
  
  return(exp)
  
}

