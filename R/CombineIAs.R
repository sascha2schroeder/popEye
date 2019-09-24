
CombineIAs <- function(exp) {
  
  # ia
  exp$out$iatmp$id <- 
    factor(exp$out$iatmp$subid):factor(exp$out$iatmp$trialnum):factor(exp$out$iatmp$ianum)
  if (exp$setup$type == "target" | exp$setup$type == "boundary" | exp$setup$type == "fast") {
    names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", 
               "sentnum", "ianum", "ia", "target", 
               "blink", "skip", "nrun", "reread", "nfix", "refix", "reg.in", "reg.out", 
               "dur", "gopast", "gopast.sel")
  } else {
    names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", 
               "sentnum", "ianum", "ia", 
               "blink", "skip", "nrun", "reread", "nfix", "refix", "reg.in", "reg.out", 
               "dur", "gopast", "gopast.sel")
  }
  iatmp <- exp$out$iatmp[names]
  
  # first
  exp$out$iafirst$id <- 
    factor(exp$out$iafirst$subid):factor(exp$out$iafirst$trialnum):factor(exp$out$iafirst$ianum)
  names <- c("id", "firstrun.skip", "firstrun.nfix", "firstrun.refix", "firstrun.reg.in", 
             "firstrun.reg.out", "firstrun.dur", "firstrun.gopast", "firstrun.gopast.sel")
  firsttmp <- exp$out$iafirst[names]
  
  
  # firstfix
  exp$out$fix$id <- 
    factor(exp$out$fix$subid):factor(exp$out$fix$trialnum):factor(exp$out$fix$ianum)
  fixtmp <- exp$out$fix[exp$out$fix$ia.run == 1 & exp$out$fix$ia.run.fix == 1, ]
  names <- c("id", "sac.in", "sac.out", "ia.launch", "ia.land", "ia.cland", "dur")
  fixtmp <- fixtmp[names]
  colnames(fixtmp) <- c("id", "firstfix.sac.in", "firstfix.sac.out", "firstfix.launch", 
                        "firstfix.land", "firstfix.cland", "firstfix.dur")

  # merge 
  comb <- merge(merge(iatmp, firsttmp, all.x = T), fixtmp, all.x = T)
  comb$id <- NULL
  
  # clean up
  exp$out$fix$id <- NULL
  exp$out$iatmp <- NULL
  exp$out$iafirst <- NULL
  
  exp$out$ias <- comb
  exp$out$ias <- exp$out$ias[order(exp$out$ias$subid, 
                                   exp$out$ias$trialnum, 
                                   exp$out$ias$ianum), ]
  row.names(exp$out$ias) <- NULL
  
  # recompute firstrun skip (skips also firstkips)
  exp$out$ias$firstrun.skip[exp$out$ias$skip == 1] <- 1
  
  # gopast time in firstrun
  exp$out$ias$firstrun.gopast <- exp$out$ias$gopast
  exp$out$ias$firstrun.gopast.sel <- exp$out$ias$gopast.sel
  exp$out$ias$gopast <- NULL
  exp$out$ias$gopast.sel <- NULL
  
  # # delete firstrun measures if firstrun.skip
  # exp$out$ias$firstrun.nfix[exp$out$ias$firstrun.skip == 1] <- NA
  # exp$out$ias$firstrun.refix[exp$out$ias$firstrun.skip == 1] <- NA
  # exp$out$ias$firstrun.reg.in[exp$out$ias$firstrun.skip == 1] <- NA  
  # exp$out$ias$firstrun.reg.out[exp$out$ias$firstrun.skip == 1] <- NA  
  # exp$out$ias$firstrun.dur[exp$out$ias$firstrun.skip == 1] <- NA  
  # exp$out$ias$firstrun.gopast[exp$out$ias$firstrun.skip == 1] <- NA  
  # exp$out$ias$firstrun.gopast.sel[exp$out$ias$firstrun.skip == 1] <- NA  

  # # delete firstfix measures if firstrun.skip
  # exp$out$ias$firstfix.sac.in[exp$out$ias$firstrun.skip == 1] <- NA
  # exp$out$ias$firstfix.sac.out[exp$out$ias$firstrun.skip == 1] <- NA
  # exp$out$ias$firstfix.launch[exp$out$ias$firstrun.skip == 1] <- NA  
  # exp$out$ias$firstfix.land[exp$out$ias$firstrun.skip == 1] <- NA  
  # exp$out$ias$firstfix.cland[exp$out$ias$firstrun.skip == 1] <- NA  
  # exp$out$ias$firstfix.dur[exp$out$ias$firstrun.skip == 1] <- NA  
  
  
  # compute single fixation measures
  # ---------------------------------
  
  # single indicator
  exp$out$ias$singlefix <- 0
  exp$out$ias$singlefix[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1] <- 1
  
  # sac.in
  exp$out$ias$singlefix.sac.in <- NA
  exp$out$ias$singlefix.sac.in[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1] <- exp$out$ias$firstfix.sac.in[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1]
  
  # sac.out
  exp$out$ias$singlefix.sac.out <- NA
  exp$out$ias$singlefix.sac.out[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1] <- exp$out$ias$firstfix.sac.out[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1]
  
  # launch
  exp$out$ias$singlefix.launch <- NA
  exp$out$ias$singlefix.launch[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1] <- exp$out$ias$firstfix.launch[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1]
  
  # land
  exp$out$ias$singlefix.land <- NA
  exp$out$ias$singlefix.land[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1] <- exp$out$ias$firstfix.land[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1]
  
  # cland
  exp$out$ias$singlefix.cland <- NA
  exp$out$ias$singlefix.cland[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1] <- exp$out$ias$firstfix.cland[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1]
  
  # duration
  exp$out$ias$singlefix.dur <- NA
  exp$out$ias$singlefix.dur[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1] <- exp$out$ias$firstfix.dur[is.na(exp$out$ias$firstrun.nfix) == F & exp$out$ias$firstrun.nfix == 1]
  
  return(exp)
  
}
