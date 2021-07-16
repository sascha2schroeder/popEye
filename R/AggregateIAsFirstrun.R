
AggregateIAsFirstrun <- function(exp) {
  
  # create outfile  
  firstruntmp <- exp$out$fix[exp$out$fix$ia.run == 1, ]
  firstruntmp$id <- as.character(paste(firstruntmp$subid, firstruntmp$trialnum, 
                                       firstruntmp$ianum, sep = ":"))
  firstrun <- firstruntmp[duplicated(firstruntmp$id) == F, ]
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia")
  firstrun <- firstrun[names]  
  firstrun <- firstrun[order(firstrun$id), ]
  
  # compute measures
  firstrun$firstrun.blink <- as.numeric(tapply(firstruntmp$blink, list(firstruntmp$id), max))
  firstrun$firstrun.skip <- as.numeric(tapply(firstruntmp$ia.firstskip, list(firstruntmp$id), max))
  firstrun$firstrun.nfix <- as.numeric(tapply(firstruntmp$fixid, list(firstruntmp$id), length))
  firstrun$firstrun.refix <- as.numeric(tapply(firstruntmp$ia.refix, list(firstruntmp$id), max))
  firstrun$firstrun.reg.in <- as.numeric(tapply(firstruntmp$ia.reg.in, list(firstruntmp$id), max))
  firstrun$firstrun.reg.out <- as.numeric(tapply(firstruntmp$ia.reg.out, list(firstruntmp$id), max))
  firstrun$firstrun.dur <- as.numeric(tapply(firstruntmp$dur, list(firstruntmp$id), sum))
  firstrun$firstrun.gopast <- NA
  firstrun$firstrun.gopast.sel <- NA
  
  # save
  firstrun <- firstrun[order(firstrun$trialnum, firstrun$ia), ]
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia", 
             "firstrun.blink", "firstrun.skip", "firstrun.nfix", "firstrun.refix", 
             "firstrun.reg.in", "firstrun.reg.out", "firstrun.dur", 
             "firstrun.gopast", "firstrun.gopast.sel")
  firstrun <- firstrun[names]
  firstrun <- firstrun[order(firstrun$subid), ]
  
  return(firstrun)
  
}
