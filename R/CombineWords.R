
CombineWords <- function(fix, wordfirst, wordtmp) {
  
  # word
  wordtmp$id <- factor(wordtmp$trialnum):factor(wordtmp$wordnum)
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "sentnum",
             "wordnum", "word", "blink", "skip", "nrun", "reread", "nfix", 
             "refix", "reg.in", "reg.out", "dur", "gopast", "gopast.sel")
  wordtmp <- wordtmp[names]
  
  # first run
  wordfirst$id <- factor(wordfirst$trialnum):factor(wordfirst$wordnum)
  names <- c("id", "firstrun.skip", "firstrun.nfix", "firstrun.refix", 
             "firstrun.reg.in", "firstrun.reg.out", "firstrun.dur", 
             "firstrun.gopast", "firstrun.gopast.sel")
  wordfirsttmp <- wordfirst[names]
  
  # first fixation
  fix$id <- factor(fix$trialnum):factor(fix$wordnum)
  fixtmp <- fix[fix$word.run == 1 & fix$word.run.fix == 1, ]
  names <- c("id", "sac.in", "sac.out", "word.launch", "word.land", "word.cland", "dur")
  fixtmp <- fixtmp[names]
  colnames(fixtmp) <- c("id", "firstfix.sac.in", "firstfix.sac.out", 
                        "firstfix.launch", "firstfix.land", 
                        "firstfix.cland", "firstfix.dur")
  
  # merge 
  comb <- merge(merge(wordtmp, wordfirsttmp, all.x = T), fixtmp, all.x = T)
  comb$id <- NULL
  
  # clean up
  fix$id <- NULL
  wordtmp <- NULL
  wordfirst <- NULL
  
  comb <- comb[order(comb$trialnum, comb$wordnum), ]
  row.names(comb) <- NULL
  
  
  # recompute firstrun skip (skips are also firstkips)
  comb$firstrun.skip[comb$skip == 1] <- 1
  
  # gopast time in firstrun
  comb$firstrun.gopast <- comb$gopast
  comb$firstrun.gopast.sel <- comb$gopast.sel
  comb$gopast <- NULL
  comb$gopast.sel <- NULL
  
  
  # # delete firstrun measures if firstrun.skip
  # comb$firstrun.nfix[comb$firstrun.skip == 1] <- NA
  # comb$firstrun.refix[comb$firstrun.skip == 1] <- NA
  # comb$firstrun.reg.in[comb$firstrun.skip == 1] <- NA  
  # comb$firstrun.reg.out[comb$firstrun.skip == 1] <- NA  
  # comb$firstrun.dur[comb$firstrun.skip == 1] <- NA  
  # comb$firstrun.gopast[comb$firstrun.skip == 1] <- NA  
  # comb$firstrun.gopast.sel[comb$firstrun.skip == 1] <- NA  
  
  # # delete firstfix measures if firstrun.skip
  # comb$firstfix.sac.in[comb$firstrun.skip == 1] <- NA
  # comb$firstfix.sac.out[comb$firstrun.skip == 1] <- NA
  # comb$firstfix.launch[comb$firstrun.skip == 1] <- NA  
  # comb$firstfix.land[comb$firstrun.skip == 1] <- NA  
  # comb$firstfix.cland[comb$firstrun.skip == 1] <- NA  
  # comb$firstfix.dur[comb$firstrun.skip == 1] <- NA  
  
  
  # compute single fixation measures
  # ---------------------------------
  
  # singlefix indicator
  comb$singlefix <- 0
  comb$singlefix[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1] <- 1
  
  # sac.in
  comb$singlefix.sac.in <- NA
  comb$singlefix.sac.in[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1] <- comb$firstfix.sac.in[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1]
  
  # sac.out
  comb$singlefix.sac.out <- NA
  comb$singlefix.sac.out[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1] <- comb$firstfix.sac.out[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1]
  
  # launch
  comb$singlefix.launch <- NA
  comb$singlefix.launch[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1] <- comb$firstfix.launch[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1]
  
  # launch
  comb$singlefix.launch <- NA
  comb$singlefix.launch[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1] <- comb$firstfix.launch[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1]
  
  # land
  comb$singlefix.land <- NA
  comb$singlefix.land[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1] <- comb$firstfix.land[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1]
  
  # cland
  comb$singlefix.cland <- NA
  comb$singlefix.cland[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1] <- comb$firstfix.cland[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1]
  
  # duration
  comb$singlefix.dur <- NA
  comb$singlefix.dur[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1] <- comb$firstfix.dur[is.na(comb$firstrun.nfix) == F & comb$firstrun.nfix == 1]
  
  return(comb)
  
}
