
AggregateSentence <- function(exp) {
  
  # create outfile  
  senttmp <- exp$out$fix[exp$out$fix$type == "in", ]
  senttmp$id <- as.character(paste(senttmp$subid, senttmp$trialnum, 
                                   senttmp$sentnum, sep = ":"))
  senttmp <- senttmp[order(senttmp$id), ]
  
  sent <- senttmp[duplicated(senttmp$id) == F, ]
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "sentnum", 
             "sent", "sent.nwords")
  sent <- sent[names]  
  
  # compute gopast time
  senttmp <- ComputeGopastSentence(senttmp)
  
  # compute measures
  sent$blink <- as.numeric(tapply(senttmp$blink, list(senttmp$id), max))
  sent$nrun <- as.numeric(tapply(senttmp$sent.run, list(senttmp$id), max))
  sent$reread <- ifelse(sent$nrun > 1, 1, 0)
  sent$nfix <- as.numeric(tapply(senttmp$fixid, list(senttmp$id), length))
  sent$refix <- as.numeric(tapply(senttmp$sent.refix, list(senttmp$id), max, na.rm = T))
  sent$reg.in <- as.numeric(tapply(senttmp$sent.reg.in, list(senttmp$id), max))
  sent$reg.out <- as.numeric(tapply(senttmp$sent.reg.out, list(senttmp$id), max))
  sent$dur <- as.numeric(tapply(senttmp$dur, list(senttmp$id), sum))
  sent$gopast <- as.numeric(tapply(senttmp$gopast, list(senttmp$id), max))
  sent$gopast.sel <- as.numeric(tapply(senttmp$selgopast, list(senttmp$id), max))
  sent$rate <- round(sent$dur / sent$sent.nwords)

  # compute skippings
  # ------------------
  
  # delete variables
  sent <- sent[, -match(c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "sent"), colnames(sent))]
  
  item <- exp$out$sent.item
  item$id <- as.character(paste(item$subid, item$trialnum, item$sentnum, sep = ":"))
  sent <- merge(sent, item, by = "id", all.y = T)
  sent$skip <- 0
  sent$skip[is.na(sent$blink) == T] <- 1
  
  # recompute blinks
  sent$blink[is.na(sent$dur)] <- 0
  
  
  # save
  # -----
  
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "sent",
             "sent.nwords", "blink", "skip", "nrun", "reread", "nfix", "refix", 
             "reg.in", "reg.out", "dur", "gopast", "gopast.sel", "rate")
  
  sent <- sent[names]
  sent <- sent[order(sent$subid, sent$trialnum, sent$sentnum), ]
  
  return(sent)
  
}
