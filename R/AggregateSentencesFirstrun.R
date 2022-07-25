
AggregateSentencesFirstrun <- function(fix) {
  
  # create outfile  
  firstruntmp <- fix[fix$sent.run == 1, ]
  firstruntmp$id <- as.character(paste(firstruntmp$trialnum, firstruntmp$sentnum, sep = ":"))
  firstrun <- firstruntmp[duplicated(firstruntmp$id) == F, ]
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "sent")
  firstrun <- firstrun[names]  
  firstrun <- firstrun[order(firstrun$id), ]
  
  # compute measures
  # firstrun$firstrun.blink <- as.numeric(tapply(firstruntmp$blink, list(firstruntmp$id), max))
  firstrun$firstrun.skip <- as.numeric(tapply(firstruntmp$sent.firstskip, list(firstruntmp$id), max))
  # firstrun$firstrun.nfix <- as.numeric(tapply(firstruntmp$fixid, list(firstruntmp$id), length))
  # firstrun$firstrun.refix <- as.numeric(tapply(firstruntmp$sent.refix, list(firstruntmp$id), max))
  firstrun$firstrun.reg.in <- as.numeric(tapply(firstruntmp$sent.reg.in, list(firstruntmp$id), max))
  firstrun$firstrun.reg.out <- as.numeric(tapply(firstruntmp$sent.reg.out, list(firstruntmp$id), max))
  # firstrun$firstrun.dur <- as.numeric(tapply(firstruntmp$dur, list(firstruntmp$id), sum))
  # firstrun$firstrun.gopast <- NA
  # firstrun$firstrun.gopast.sel <- NA

  
  # save
  firstrun <- firstrun[order(firstrun$trialnum, firstrun$sentnum), ]
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "sent",
             "firstrun.skip", "firstrun.reg.in", "firstrun.reg.out")
  firstrun <- firstrun[names]
  
  return(firstrun)
  
}
