
AggregateIAFirstrun <- function(exp) {
  
  # create outfile  
  firstruntmp <- exp$out$fix[exp$out$fix$ia.run == 1, ]
  firstruntmp$id <- as.character(paste(firstruntmp$subid, firstruntmp$trialnum, 
                                       firstruntmp$ianum, sep = ":"))
  firstrun <- firstruntmp[duplicated(firstruntmp$id) == F, ]
  names <- c("id", "subid", "trialnum", "ianum")
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
  
  # delete variables
  firstrun <- firstrun[, -match(c("subid", "trialnum", "ianum"), colnames(firstrun))]
  
  # merge with item file
  item <- exp$out$ia.item
  item$id <- as.character(paste(item$subid, item$trialnum, item$ianum, sep = ":"))
  firstrun <- merge(firstrun, item, by = "id", all.y = T)
  firstrun <- firstrun[is.na(firstrun$firstrun.dur) == F, ]
  
  # save
  firstrun <- firstrun[order(firstrun$trialnum, firstrun$ia), ]
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia", 
             "firstrun.blink", "firstrun.skip", "firstrun.nfix", "firstrun.refix", 
             "firstrun.reg.in", "firstrun.reg.out", "firstrun.dur")
  firstrun <- firstrun[names]
  firstrun <- firstrun[order(firstrun$subid), ]
  
  return(firstrun)
  
}
