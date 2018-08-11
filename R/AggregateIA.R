
AggregateIA <- function(exp) {
  
  # create outfile  
  iatmp <- exp$out$fix
  iatmp$id <- as.character(paste(iatmp$subid, iatmp$trialnum, iatmp$ianum, sep = ":"))
  ia <- iatmp[duplicated(iatmp$id) == F, ]
  names <- c("id", "subid", "trialid", "trialnum", "ianum")
  ia <- ia[names]  
  ia <- ia[order(ia$id), ]
  
  # compute gopast time
  iatmp <- ComputeGopastIA(iatmp)
  
  # compute measures
  ia$blink <- as.numeric(tapply(iatmp$blink, list(iatmp$id), max))
  ia$nrun <- as.numeric(tapply(iatmp$ia.run, list(iatmp$id), max))
  ia$reread <- ifelse(ia$nrun > 1, 1, 0)
  ia$nfix <- as.numeric(tapply(iatmp$fixid, list(iatmp$id), length))
  ia$refix <- as.numeric(tapply(iatmp$ia.refix, list(iatmp$id), max))
  ia$reg.in <- as.numeric(tapply(iatmp$ia.reg.in, list(iatmp$id), max))
  ia$reg.out <- as.numeric(tapply(iatmp$ia.reg.out, list(iatmp$id), max))
  ia$dur <- as.numeric(tapply(iatmp$dur, list(iatmp$id), sum))
  ia$gopast <- as.numeric(tapply(iatmp$gopast, list(iatmp$id), max))
  ia$gopast.sel <- as.numeric(tapply(iatmp$selgopast, list(iatmp$id), max))
  
  # delete variables
  ia <- ia[, -match(c("subid", "trialid", "trialnum", "ianum"), colnames(ia))]
  
  # skippings
  item <- exp$out$ia.item
  item$id <- as.character(paste(item$subid, item$trialnum, item$ianum, sep = ":"))
  ia <- merge(ia, item, by = "id", all.y = T)
  ia$skip <- 0
  ia$skip[is.na(ia$blink)] <- 1
  
  # recompute blink
  ia$blink[is.na(ia$dur)] <- 0
  
  # save
  ia <- ia[order(ia$trialnum, ia$ia), ]
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia", 
             "blink", "skip", "nrun", "reread", "nfix", "refix", "reg.in", 
             "reg.out", "dur", "gopast", "gopast.sel")
  ia <- ia[names]
  
  return(ia)
  
}
