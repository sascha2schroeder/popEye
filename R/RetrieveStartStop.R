
RetrieveStartStop <- function(dat, trial) {
  
  ntrial <- unlist(dimnames(table(dat$msg$trialnum)))
  
  start <- min(dat$msg$time[dat$msg$trialnum == ntrial[trial]])
  stop <- max(dat$msg$time[dat$msg$trialnum == ntrial[trial]])
  
  out <- list(start = start, stop = stop)
  
  return(out)
  
}
