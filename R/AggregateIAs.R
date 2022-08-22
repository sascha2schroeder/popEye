
AggregateIAs <- function(fix, ia.item, exp) {
  
  # create outfile  
  iatmp <- fix[fix$type == "in", ]
  iatmp$id <- as.character(paste(iatmp$trialnum, iatmp$ianum, sep = ":"))
  ia <- iatmp[duplicated(iatmp$id) == F, ]
  
  if (exp$setup$type == "target" | exp$setup$type == "boundary" | exp$setup$type == "fast") {
    names <- c("id", "subid", "trialid", "itemid", "cond", "trialnum", 
               "sentnum", "ianum", "ia", "target")
  } else {
    names <- c("id", "subid", "trialid", "itemid", "cond", "trialnum", 
               "sentnum", "ianum", "ia")
  }
  
  ia <- ia[names]  
  ia <- ia[order(ia$id), ]
  
  # compute gopast time
  iatmp <- ComputeGopastIA(iatmp)
  
  # compute measures
  ia$blink <- as.numeric(tapply(iatmp$blink, list(iatmp$id), max, na.rm = T))
  ia$nrun <- as.numeric(tapply(iatmp$ia.run, list(iatmp$id), max, na.rm = T))
  ia$reread <- ifelse(ia$nrun > 1, 1, 0)
  ia$nfix <- as.numeric(tapply(iatmp$fixid, list(iatmp$id), length))
  ia$refix <- as.numeric(tapply(iatmp$ia.refix, list(iatmp$id), max, na.rm = T))
  ia$reg.in <- as.numeric(tapply(iatmp$ia.reg.in, list(iatmp$id), max, na.rm = T))
  ia$reg.out <- as.numeric(tapply(iatmp$ia.reg.out, list(iatmp$id), max, na.rm = T))
  ia$dur <- as.numeric(tapply(iatmp$dur, list(iatmp$id), sum, na.rm = T))
  ia$gopast <- as.numeric(tapply(iatmp$gopast, list(iatmp$id), max, na.rm = T))
  ia$gopast.sel <- as.numeric(tapply(iatmp$selgopast, list(iatmp$id), max, na.rm = T))
  
  
  # compute skippings
  # ------------------
  
  # delete variables
  if (exp$setup$type == "target" | exp$setup$type == "boundary" | exp$setup$type == "fast") {
    ia <- ia[, -match(c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum",
                        "ianum", "ia", "target"), colnames(ia))]
  } else {
    ia <- ia[, -match(c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum",
                        "ianum", "ia"), colnames(ia))]
  }
  
  item <- ia.item
  item$id <- as.character(paste(item$trialnum, item$ianum, sep = ":"))
  ia <- merge(ia, item, by = "id", all.y = T)
  
  ia$skip <- 0
  ia$skip[is.na(ia$blink) == T] <- 1
  
  # recompute blinks
  ia$blink[is.na(ia$dur)] <- 0
  
  
  # save
  # -----
  
  if (exp$setup$type == "target" | exp$setup$type == "boundary" | exp$setup$type == "fast") {
    names <- c("subid", "trialid", "trialnum", "itemid", "cond", 
               "sentnum", "ianum", "ia",
               "target", "blink", "skip", "nrun", "reread", "nfix", "refix",
               "reg.in", "reg.out", "dur", "gopast", "gopast.sel")
  } else {
    names <- c("subid", "trialid", "trialnum", "itemid", "cond", 
               "sentnum", "ianum", "ia", 
               "blink", "skip", "nrun", "reread", "nfix", "refix", 
               "reg.in", "reg.out", "dur", "gopast", "gopast.sel")
  }
  
  ia <- ia[names]
  ia <- ia[order(ia$subid, ia$trialid, ia$ianum), ]
  
  return(ia)
  
}

