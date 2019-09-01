
CombineWords <- function(exp) {
  
  # word
  exp$out$wordtmp$id <- 
    factor(exp$out$wordtmp$subid):factor(exp$out$wordtmp$trialnum):factor(exp$out$wordtmp$wordnum)
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "sentnum",
             "wordnum", "word", "blink", "skip", "nrun", "reread", "nfix", "refix", "reg.in", 
             "reg.out", "dur", "gopast", "gopast.sel")
  wordtmp <- exp$out$wordtmp[names]
  
  # first run
  exp$out$wordfirst$id <- 
    factor(exp$out$wordfirst$subid):factor(exp$out$wordfirst$trialnum):factor(exp$out$wordfirst$wordnum)
  names <- c("id", "firstrun.skip", "firstrun.nfix", "firstrun.refix", 
             "firstrun.reg.in", "firstrun.reg.out", "firstrun.dur", 
             "firstrun.gopast", "firstrun.gopast.sel")
  wordfirsttmp <- exp$out$wordfirst[names]
  
  # first fixation
  exp$out$fix$id <- 
    factor(exp$out$fix$subid):factor(exp$out$fix$trialnum):factor(exp$out$fix$wordnum)
  fixtmp <- exp$out$fix[exp$out$fix$word.run == 1 & exp$out$fix$word.run.fix == 1, ]
  names <- c("id", "sac.in", "sac.out", "word.launch", "word.land", "word.cland", "dur")
  fixtmp <- fixtmp[names]
  colnames(fixtmp) <- c("id", "firstfix.sac.in", "firstfix.sac.out", 
                        "firstfix.launch", "firstfix.land", 
                        "firstfix.cland", "firstfix.dur")
  
  # merge 
  comb <- merge(merge(wordtmp, wordfirsttmp, all.x = T), fixtmp, all.x = T)
  comb$id <- NULL
  
  # clean up
  exp$out$fix$id <- NULL
  exp$out$wordtmp <- NULL
  exp$out$wordfirst <- NULL
  
  exp$out$words <- comb
  exp$out$words <- exp$out$words[order(exp$out$words$subid, 
                                       exp$out$words$trialnum, 
                                       exp$out$words$wordnum), ]
  row.names(exp$out$words) <- NULL
  
  
  # recompute firstrun skip (skips are also firstkips)
  exp$out$words$firstrun.skip[exp$out$words$skip == 1] <- 1
  
  # gopast time in firstrun
  exp$out$words$firstrun.gopast <- exp$out$words$gopast
  exp$out$words$firstrun.gopast.sel <- exp$out$words$gopast.sel
  exp$out$words$gopast <- NULL
  exp$out$words$gopast.sel <- NULL
  
  # delete firstrun measures if firstrun.blink
  exp$out$words$firstrun.nfix[exp$out$words$firstrun.skip == 1] <- NA
  exp$out$words$firstrun.refix[exp$out$words$firstrun.skip == 1] <- NA
  exp$out$words$firstrun.reg.in[exp$out$words$firstrun.skip == 1] <- NA  
  exp$out$words$firstrun.reg.out[exp$out$words$firstrun.skip == 1] <- NA  
  exp$out$words$firstrun.dur[exp$out$words$firstrun.skip == 1] <- NA  
  exp$out$words$firstrun.gopast[exp$out$words$firstrun.skip == 1] <- NA  
  exp$out$words$firstrun.gopast.sel[exp$out$words$firstrun.skip == 1] <- NA  
  
  # delete firstfix measures if firstrun.blink
  exp$out$words$firstfix.sac.in[exp$out$words$firstrun.skip == 1] <- NA
  exp$out$words$firstfix.sac.out[exp$out$words$firstrun.skip == 1] <- NA
  exp$out$words$firstfix.launch[exp$out$words$firstrun.skip == 1] <- NA  
  exp$out$words$firstfix.land[exp$out$words$firstrun.skip == 1] <- NA  
  exp$out$words$firstfix.cland[exp$out$words$firstrun.skip == 1] <- NA  
  exp$out$words$firstfix.dur[exp$out$words$firstrun.skip == 1] <- NA  
  
  
  # compute single fixation measures
  # ---------------------------------
  
  # singlefix indicator
  exp$out$words$singlefix <- 0
  exp$out$words$singlefix[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1] <- 1
  
  # sac.in
  exp$out$words$singlefix.sac.in <- NA
  exp$out$words$singlefix.sac.in[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1] <- exp$out$words$firstfix.sac.in[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1]
  
  # sac.out
  exp$out$words$singlefix.sac.out <- NA
  exp$out$words$singlefix.sac.out[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1] <- exp$out$words$firstfix.sac.out[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1]
  
  # launch
  exp$out$words$singlefix.launch <- NA
  exp$out$words$singlefix.launch[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1] <- exp$out$words$firstfix.launch[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1]
  
  # launch
  exp$out$words$singlefix.launch <- NA
  exp$out$words$singlefix.launch[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1] <- exp$out$words$firstfix.launch[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1]
  
  # land
  exp$out$words$singlefix.land <- NA
  exp$out$words$singlefix.land[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1] <- exp$out$words$firstfix.land[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1]
  
  # cland
  exp$out$words$singlefix.cland <- NA
  exp$out$words$singlefix.cland[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1] <- exp$out$words$firstfix.cland[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1]
  
  # duration
  exp$out$words$singlefix.dur <- NA
  exp$out$words$singlefix.dur[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1] <- exp$out$words$firstfix.dur[is.na(exp$out$words$firstrun.nfix) == F & exp$out$words$firstrun.nfix == 1]
  
  return(exp)
  
}
