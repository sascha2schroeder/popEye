
CombineIA <- function(exp) {
  
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
  
  exp$out$ia <- comb
  exp$out$ia <- exp$out$ia[order(exp$out$ia$subid, exp$out$ia$trialnum, exp$out$ia$ianum), ]
  row.names(exp$out$ia) <- NULL
  
  # recompute firstrun skip (skips also firstkips)
  exp$out$ia$firstrun.skip[exp$out$ia$skip == 1] <- 1
  
  # gopast time in firstrun
  exp$out$ia$firstrun.gopast <- exp$out$ia$gopast
  exp$out$ia$firstrun.gopast.sel <- exp$out$ia$gopast.sel
  exp$out$ia$gopast <- NULL
  exp$out$ia$gopast.sel <- NULL
  
  
  # compute single fixation measures
  # ---------------------------------
  
  # single indicator
  exp$out$ia$singlefix <- 0
  exp$out$ia$singlefix[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1] <- 1
  exp$out$ia$singlefix[exp$out$ia$skip == 1] <- NA
  
  # sac.in
  exp$out$ia$singlefix.sac.in <- NA
  exp$out$ia$singlefix.sac.in[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1] <- exp$out$ia$firstfix.sac.in[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1]
  
  # sac.out
  exp$out$ia$singlefix.sac.out <- NA
  exp$out$ia$singlefix.sac.out[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1] <- exp$out$ia$firstfix.sac.out[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1]
  
  # launch
  exp$out$ia$singlefix.launch <- NA
  exp$out$ia$singlefix.launch[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1] <- exp$out$ia$firstfix.launch[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1]
  
  # land
  exp$out$ia$singlefix.land <- NA
  exp$out$ia$singlefix.land[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1] <- exp$out$ia$firstfix.land[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1]
  
  # cland
  exp$out$ia$singlefix.cland <- NA
  exp$out$ia$singlefix.cland[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1] <- exp$out$ia$firstfix.cland[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1]
  
  # duration
  exp$out$ia$singlefix.dur <- NA
  exp$out$ia$singlefix.dur[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1] <- exp$out$ia$firstfix.dur[is.na(exp$out$ia$firstrun.nfix) == F & exp$out$ia$firstrun.nfix == 1]
  
  
  # delete firstrun and singlefix variables for firstskips
  # ------------------------------------------------------
  
  # exp$out$ia$firstrun.gopast[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstrun.gopast.sel[exp$out$ia$firstrun.skip == 1] <- NA
  # 
  # exp$out$ia$firstrun.nfix[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstrun.refix[exp$out$ia$firstrun.skip == 1] <- NA
  # 
  # exp$out$ia$firstrun.reg.in[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstrun.reg.out[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstrun.dur[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstfix.sac.in[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstfix.sac.out[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstfix.launch[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstfix.land[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstfix.cland[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$firstfix.dur[exp$out$ia$firstrun.skip == 1] <- NA
  # 
  # exp$out$ia$singlefix[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$singlefix.sac.in[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$singlefix.sac.out[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$singlefix.launch[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$singlefix.land[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$singlefix.cland[exp$out$ia$firstrun.skip == 1] <- NA
  # exp$out$ia$singlefix.dur[exp$out$ia$firstrun.skip == 1] <- NA
  
  return(exp)
  
}
