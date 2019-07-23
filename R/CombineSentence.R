
CombineSentence <- function(exp) {
  
  # sentence
  exp$out$senttmp$id <- 
    factor(exp$out$senttmp$subid):factor(exp$out$senttmp$trialnum):factor(exp$out$senttmp$sentnum)
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "sentnum", 
             "sent", "sent.nwords", "blink", "skip", "nrun", "reread", "nfix", 
             "refix", "reg.in", "reg.out", "dur", "gopast", "gopast.sel", "rate")
  senttmp <- exp$out$senttmp[names]
  
  # first
  exp$out$sentfirst$id <- 
    factor(exp$out$sentfirst$subid):factor(exp$out$sentfirst$trialnum):factor(exp$out$sentfirst$sentnum)
  names <- c("id", "firstrun.nfix", "firstrun.refix", "firstrun.reg.in", 
             "firstrun.reg.out", "firstrun.dur", "firstrun.gopast", "firstrun.gopast.sel")
  firsttmp <- exp$out$sentfirst[names]
  
  # merge 
  comb <- merge(senttmp, firsttmp, all.x = T)
  comb$id <- NULL
  
  # clean up
  exp$out$senttmp <- NULL
  exp$out$sentfirst <- NULL
  
  exp$out$sent <- comb
  exp$out$sent <- exp$out$sent[order(exp$out$sent$subid, exp$out$sent$trialnum, exp$out$sent$sentnum), ]
  row.names(exp$out$sent) <- NULL
  
  # gopast time in firstrun
  exp$out$sent$firstrun.gopast <- exp$out$sent$gopast
  exp$out$sent$firstrun.gopast.sel <- exp$out$sent$gopast.sel
  exp$out$sent$gopast <- NULL
  exp$out$sent$gopast.sel <- NULL
  
  return(exp)
  
}
