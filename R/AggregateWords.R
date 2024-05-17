
AggregateWords <- function(fix, word.item) {
  
  # create outfile  
  wordtmp <- fix[fix$type == "in", ]
  wordtmp$id <- as.character(paste(wordtmp$trialnum, wordtmp$wordnum, sep = ":"))
  
  word <- wordtmp[duplicated(wordtmp$id) == F, ]
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", 
             "sentnum", "wordnum", "word")
  word <- word[names]  
  word <- word[order(word$id), ]
  
  # compute gopast time
  wordtmp <- ComputeGopastWord(wordtmp)
  
  # compute measures
  word$blink <- as.numeric(tapply(wordtmp$blink, list(wordtmp$id), max))
  word$nrun <- as.numeric(tapply(wordtmp$word.run, list(wordtmp$id), max))
  word$reread <- ifelse(word$nrun > 1, 1, 0)
  word$nfix <- as.numeric(tapply(wordtmp$fixid, list(wordtmp$id), length))
  word$refix <- as.numeric(tapply(wordtmp$word.refix, list(wordtmp$id), max))
  word$reg.in <- as.numeric(tapply(wordtmp$word.reg.in, list(wordtmp$id), max))
  word$reg.out <- as.numeric(tapply(wordtmp$word.reg.out, list(wordtmp$id), max))
  word$dur <- as.numeric(tapply(wordtmp$dur, list(wordtmp$id), sum))
  word$gopast <- as.numeric(tapply(wordtmp$gopast, list(wordtmp$id), max))
  word$gopast.sel <- as.numeric(tapply(wordtmp$selgopast, list(wordtmp$id), max))
  
  # compute skippings
  word <- word[, -match(c("subid", "trialid", "trialnum", "itemid", "cond", 
                          "sentnum", "wordnum", "word"), colnames(word))]
  
  item <- word.item
  item$id <- as.character(paste(item$trialnum, item$wordnum, sep = ":"))
  word <- merge(word, item, by = "id", all.y = T)
  word$skip <- 0
  word$skip[is.na(word$blink) == T] <- 1
  
  # NOTE: Maybe delete gopast for last word in sentence(?)
  
  # recompute blinks
  word$blink[is.na(word$dur)] <- 0
  
  # save
  word <- word[order(word$trialnum, word$wordnum), ]
  names <- c("subid", "trialid", "trialnum", "itemid", "cond",
             "sentnum", "wordnum", "word", "blink", "skip", "nrun", "reread", 
             "nfix", "refix", "reg.in", "reg.out", "dur", 
             "gopast", "gopast.sel")
  
  word <- word[names]
  word <- word[order(word$subid, word$trialid, word$wordnum), ]
  
  return(word)
  
}
