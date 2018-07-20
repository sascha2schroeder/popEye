
AggregateFirstrun <- function(exp) {
  
  # create outfile  
  firstruntmp <- exp$out$fix[exp$out$fix$ia.run == 1, ]
  firstruntmp$id <- as.character(paste(firstruntmp$subid, firstruntmp$trialnum, 
                                       firstruntmp$ia, sep = ":"))
  firstrun <- firstruntmp[duplicated(firstruntmp$id) == F, ]
  names <- c("id", "subid", "trialnum", "ia")
  firstrun <- firstrun[names]  
  firstrun <- firstrun[order(firstrun$id), ]
  
  # compute measures
  firstrun$blink <- as.numeric(tapply(firstruntmp$blink, list(firstruntmp$id), max))
  firstrun$firstskip <- as.numeric(tapply(firstruntmp$firstskip, list(firstruntmp$id), max))
  firstrun$nfix <- as.numeric(tapply(firstruntmp$fixid, list(firstruntmp$id), length))
  firstrun$refix <- as.numeric(tapply(firstruntmp$refix, list(firstruntmp$id), max))
  firstrun$reg.in <- as.numeric(tapply(firstruntmp$reg.in, list(firstruntmp$id), max))
  firstrun$reg.out <- as.numeric(tapply(firstruntmp$reg.out, list(firstruntmp$id), max))
  firstrun$dur <- as.numeric(tapply(firstruntmp$dur, list(firstruntmp$id), sum))
  
  # delete variables
  firstrun <- firstrun[, -match(c("subid", "trialnum", "ia"), colnames(firstrun))]
  
  # skippings
  item <- exp$out$item
  item$id <- as.character(paste(item$subid, item$trialnum, item$ia, sep = ":"))
  firstrun <- merge(firstrun, item, by = "id", all.y = T)
  firstrun$skip <- 0
  firstrun$skip[is.na(firstrun$blink)] <- 1
  
  # recompute structural variables
  firstrun$blink[is.na(firstrun$dur)] <- 0
  firstrun$firstskip[firstrun$skip == 1] <- 1
  
  # save
  firstrun <- firstrun[order(firstrun$trialnum, firstrun$ia), ]
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "ia", "word", 
             "blink", "skip", "firstskip", "nfix", "refix", "reg.in", "reg.out", 
             "dur")
  firstrun <- firstrun[names]
  firstrun <- firstrun[order(firstrun$subid), ]
  
  return(firstrun)
  
}
