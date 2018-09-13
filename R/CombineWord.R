
CombineWord <- function(exp) {
  
  # word
  exp$out$wordtmp$id <- 
    factor(exp$out$wordtmp$subid):factor(exp$out$wordtmp$trialnum):factor(exp$out$wordtmp$wordnum)
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "wordnum", 
             "word", "blink", "skip", "nrun", "reread", "nfix", "refix", "reg.in", 
             "reg.out", "dur", "gopast", "gopast.sel")
  wordtmp <- exp$out$wordtmp[names]

  # first run
  exp$out$wordfirst$id <- 
    factor(exp$out$wordfirst$subid):factor(exp$out$wordfirst$trialnum):factor(exp$out$wordfirst$wordnum)
  names <- c("id", "firstrun.skip", "firstrun.nfix", "firstrun.refix", 
             "firstrun.reg.in", "firstrun.reg.out", "firstrun.dur")
  wordfirsttmp <- exp$out$wordfirst[names]
  
  # first fixation
  exp$out$fix$id <- 
    factor(exp$out$fix$subid):factor(exp$out$fix$trialnum):factor(exp$out$fix$wordnum)
  fixtmp <- exp$out$fix[exp$out$fix$word.run == 1 & exp$out$fix$word.run.fix == 1, ]
  names <- c("id", "word.launch", "word.land", "word.cland", "dur")
  fixtmp <- fixtmp[names]
  colnames(fixtmp) <- c("id", "firstfix.launch", "firstfix.land", 
                        "firstfix.cland", "firstfix.dur")

  # merge 
  comb <- merge(merge(wordtmp, wordfirsttmp, all.x = T), fixtmp, all.x = T)
  comb$id <- NULL
  
  # clean up
  exp$out$fix$id <- NULL
  exp$out$wordtmp <- NULL
  exp$out$wordfirst <- NULL
  
  exp$out$word <- comb
  exp$out$word <- exp$out$word[order(exp$out$word$subid, exp$out$word$trialnum, exp$out$word$wordnum), ]
  row.names(exp$out$word) <- NULL
  
  # recompute firstrun skip (skips also firstkips)
  # exp$out$word$firstrun.skip[exp$out$word$skip == 1] <- 1
  # NOTE: does this make sense?
  exp$out$word$firstrun.skip[exp$out$word$skip == 1] <- NA
  
  # compute single measures
  
  # single
  exp$out$word$singlefix <- 0
  exp$out$word$singlefix[is.na(exp$out$word$firstrun.nfix) == F & exp$out$word$firstrun.nfix == 1] <- 1
  exp$out$word$singlefix[exp$out$word$skip == 1] <- NA
  
  # launch
  exp$out$word$singlefix.launch <- NA
  exp$out$word$singlefix.launch[is.na(exp$out$word$firstrun.nfix) == F & exp$out$word$firstrun.nfix == 1] <- exp$out$word$firstfix.launch[is.na(exp$out$word$firstrun.nfix) == F & exp$out$word$firstrun.nfix == 1]
  
  # land
  exp$out$word$singlefix.land <- NA
  exp$out$word$singlefix.land[is.na(exp$out$word$firstrun.nfix) == F & exp$out$word$firstrun.nfix == 1] <- exp$out$word$firstfix.land[is.na(exp$out$word$firstrun.nfix) == F & exp$out$word$firstrun.nfix == 1]
  
  # cland
  exp$out$word$singlefix.cland <- NA
  exp$out$word$singlefix.cland[is.na(exp$out$word$firstrun.nfix) == F & exp$out$word$firstrun.nfix == 1] <- exp$out$word$firstfix.cland[is.na(exp$out$word$firstrun.nfix) == F & exp$out$word$firstrun.nfix == 1]
  
  # duration
  exp$out$word$singlefix.dur <- NA
  exp$out$word$singlefix.dur[is.na(exp$out$word$firstrun.nfix) == F & exp$out$word$firstrun.nfix == 1] <- exp$out$word$firstfix.dur[is.na(exp$out$word$firstrun.nfix) == F & exp$out$word$firstrun.nfix == 1]
  
  return(exp)
  
}
