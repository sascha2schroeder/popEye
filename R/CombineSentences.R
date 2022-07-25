
# CombineSentences <- function(fix, sentms, sentfirst, senttmp) {
CombineSentences <- function(fix, sentms, senttmp) {
    
  # sentence
  senttmp$id <- factor(senttmp$trialnum):factor(senttmp$sentnum)
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "sentnum", 
             "sent", "sent.nwords", "skip", "nrun", "reread", "reg.in", "reg.out", "rate")
  senttmp <- senttmp[names]
  
  
  # sentence measures
  sentms$id <- factor(sentms$trialnum):factor(sentms$sentnum)
  names <- c("id", "firstrun.skip", 
             "total.nfix", "total.dur", 
             "firstpass.nfix", "firstpass.dur",
             "firstpass.forward.nfix", "firstpass.forward.dur", 
             "firstpass.reread.nfix", "firstpass.reread.dur",
             "lookback.nfix", "lookback.dur",
             "lookfrom.nfix", "lookfrom.dur")
  sentms <- sentms[names]
  
  
  # # first
  # sentfirst$id <- factor(sentfirst$trialnum):factor(sentfirst$sentnum)
  # names <- c("id", "firstrun.skip", "firstrun.reg.in", "firstrun.reg.out")
  # sentfirst <- sentfirst[names]
  
  # merge  
  comb <- merge(senttmp, sentms, all.x = T)
  # comb <- merge(merge(senttmp, sentfirst, all.x = T), sentms, all.x = T)
  comb$id <- NULL
  
  # clean up
  senttmp <- NULL
  sentfirst <- NULL
  
  comb <- comb[order(comb$trialnum, comb$sentnum), ]
  row.names(comb) <- NULL
  
  return(comb)
  
}
