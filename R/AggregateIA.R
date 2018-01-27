
AggregateIA <- function(exp) {

  # create outfile  
  iatmp <- exp$out$fix
  iatmp$id <- as.character(paste(iatmp$subid, iatmp$trialnum, iatmp$ia, sep = ":"))
  ia <- iatmp[duplicated(iatmp$id) == F, ]
  names <- c("id", "subid", "trialnum", "ia")
  ia <- ia[names]  
  ia <- ia[order(ia$id), ]
  
  # compute measures
  ia$blink <- as.numeric(tapply(iatmp$blink, list(iatmp$id), max))
  ia$firstskip <- as.numeric(tapply(iatmp$firstskip, list(iatmp$id), max))
  ia$nrun <- as.numeric(tapply(iatmp$ia.run, list(iatmp$id), max))
  ia$reread <- ifelse(ia$nrun > 1, 1, 0)
  ia$nfix <- as.numeric(tapply(iatmp$fixid, list(iatmp$id), length))
  ia$refix <- as.numeric(tapply(iatmp$refix, list(iatmp$id), max))
  ia$reg <- as.numeric(tapply(iatmp$reg.in, list(iatmp$id), max))
  ia$dur <- as.numeric(tapply(iatmp$dur, list(iatmp$id), sum))
  
  # delete variables
  ia <- ia[, -match(c("subid", "trialnum", "ia"), colnames(ia))]
  
  # skippings
  item <- exp$out$item
  item$id <- as.character(paste(item$subid, item$trialnum, item$ia, sep = ":"))
  ia <- merge(ia, item, by = "id", all.y = T)
  ia$skip <- 0
  ia$skip[is.na(ia$blink)] <- 1
  
  # recompute structural variables
  ia$blink[is.na(ia$dur)] <- 0
  ia$firstskip[ia$skip == 1] <- 1
  
  # save
  ia <- ia[order(ia$trialnum, ia$ia), ]
  names <- c("subid", "trialnum", "itemid", "cond", "ia", "word", "blink", 
             "skip", "firstskip", "nrun", "reread", "nfix", "refix", "reg", "dur")
  ia <- ia[names]
  
    
  return(ia)
  
}
