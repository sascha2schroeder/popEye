
AggregateIA <- function(exp) {
  
  # create outfile  
  iatmp <- exp$out$fix
  iatmp$id <- as.character(paste(iatmp$subid, iatmp$trialnum, iatmp$ianum, sep = ":"))
  ia <- iatmp[duplicated(iatmp$id) == F, ]
  
  if (exp$setup$type == "target" | exp$setup$type == "boundary" | exp$setup$type == "fast") {
    names <- c("id", "subid", "trialid", "itemid", "cond", "trialnum", "ianum", "ia", "target")
  } else {
    names <- c("id", "subid", "trialid", "itemid", "cond", "trialnum", "ianum", "ia")
  }
  
  ia <- ia[names]  
  ia <- ia[order(ia$id), ]
  
  # compute gopast time
  iatmp <- ComputeGopastIA(iatmp)
  
  # compute measures
  ia$blink <- as.numeric(tapply(iatmp$blink, list(iatmp$id), max, na.rm = T))
  ia$nrun <- as.numeric(tapply(iatmp$ia.run, list(iatmp$id), max, na.rm = T))
  ia$reread <- ifelse(ia$nrun > 1, 1, 0)
  ia$skip <- as.numeric(tapply(iatmp$ia.skip, list(iatmp$id), max))
  ia$nfix <- as.numeric(tapply(iatmp$fixid, list(iatmp$id), length))
  ia$refix <- as.numeric(tapply(iatmp$ia.refix, list(iatmp$id), max))
  ia$reg.in <- as.numeric(tapply(iatmp$ia.reg.in, list(iatmp$id), max, na.rm = T))
  ia$reg.out <- as.numeric(tapply(iatmp$ia.reg.out, list(iatmp$id), max, na.rm = T))
  ia$dur <- as.numeric(tapply(iatmp$dur, list(iatmp$id), sum, na.rm = T))
  ia$gopast <- as.numeric(tapply(iatmp$gopast, list(iatmp$id), max, na.rm = T))
  ia$gopast.sel <- as.numeric(tapply(iatmp$selgopast, list(iatmp$id), max, na.rm = T))
  
  
  # save
  ia <- ia[order(ia$trialnum, ia$ianum), ]
  
  if (exp$setup$type == "text" | exp$setup$type == "sentence") {
    names <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia", 
               "blink", "skip", "nrun", "reread", "nfix", "refix", "reg.in", 
               "reg.out", "dur", "gopast", "gopast.sel")
  }
  
  if (exp$setup$type == "target" | exp$setup$type == "boundary" | exp$setup$type == "fast") {
    names <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia",
               "target", "blink", "skip", "nrun", "reread", "nfix", "refix", 
               "reg.in", "reg.out", "dur", "gopast", "gopast.sel")
  }
  
  ia <- ia[names]
  
  return(ia)
  
}
