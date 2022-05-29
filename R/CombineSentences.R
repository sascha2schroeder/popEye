
CombineSentences <- function(fix, sentfirst, senttmp) {
  
  # sentence
  senttmp$id <- factor(senttmp$trialnum):factor(senttmp$sentnum)
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "sentnum", 
             "sent", "sent.nwords", "blink", "skip", "nrun", "reread", "nfix", 
             "refix", "reg.in", "reg.out", "dur", "gopast", "gopast.sel", "rate")
  senttmp <- senttmp[names]
  
  # first
  sentfirst$id <- factor(sentfirst$trialnum):factor(sentfirst$sentnum)
  names <- c("id", "firstrun.nfix", "firstrun.refix", "firstrun.reg.in", 
             "firstrun.reg.out", "firstrun.dur", "firstrun.gopast", "firstrun.gopast.sel")
  sentfirst <- sentfirst[names]
  
  # merge 
  comb <- merge(senttmp, sentfirst, all.x = T)
  comb$id <- NULL
  
  # clean up
  senttmp <- NULL
  sentfirst <- NULL
  
  comb <- comb[order(comb$trialnum, comb$sentnum), ]
  row.names(comb) <- NULL
  
  # gopast time in firstrun
  comb$firstrun.gopast <- comb$gopast
  comb$firstrun.gopast.sel <- comb$gopast.sel
  comb$gopast <- NULL
  comb$gopast.sel <- NULL
  
  return(comb)
  
}
