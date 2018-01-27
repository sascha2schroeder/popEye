
CombineIA <- function(exp) {
  
  # ia
  exp$out$iatmp$id <- 
    factor(exp$out$iatmp$subid):factor(exp$out$iatmp$trialnum):factor(exp$out$iatmp$ia)
  names <- c("id", "subid", "trialnum", "itemid", "cond", "ia", "word", 
             "blink", "skip", "firstskip", "nrun", "reread", "nfix", "refix", 
             "reg", "dur")
  iatmp <- exp$out$iatmp[names]
  colnames(iatmp) <- c("id", "subid", "trialnum", "itemid", "cond", "ia", 
                       "word", "ia.blink", "ia.skip", "ia.firstskip", 
                       "ia.nrun", "ia.reread", "ia.nfix", "ia.refix", "ia.reg", 
                       "ia.dur")
  
  # first
  exp$out$first$id <- 
    factor(exp$out$first$subid):factor(exp$out$first$trialnum):factor(exp$out$first$ia)
  names <- c("id", "nfix", "refix", "reg", "dur")
  firsttmp <- exp$out$first[names]
  colnames(firsttmp) <- c("id", "firstrun.nfix", "firstrun.refix", 
                          "firstrun.reg", "firstrun.dur")
  
  # fix
  exp$out$fix$id <- 
    factor(exp$out$fix$subid):factor(exp$out$fix$trialnum):factor(exp$out$fix$ia)
  fixtmp <- exp$out$fix[exp$out$fix$ia.run == 1 & exp$out$fix$ia.run.fix == 1, ]
  names <- c("id", "launch", "land", "dur")
  fixtmp <- fixtmp[names]
  colnames(fixtmp) <- c("id", "firstfix.launch", "firstfix.land", "firstfix.dur")
  
  # merge 
  comb <- merge(merge(iatmp, firsttmp), fixtmp, all.x = T)
  comb$id <- NULL
  
  # clean up
  exp$out$fix$id <- NULL
  exp$out$iatmp <- NULL
  exp$out$first <- NULL
  
  exp$out$ia <- comb
  exp$out$ia <- exp$out$ia[order(exp$out$ia$subid, exp$out$ia$trialnum, exp$out$ia$ia), ]
  row.names(exp$out$ia) <- NULL
  
  return(exp)
  
}

