
SelectTrial <- function(dat, start = start, stop = stop) {
  
  dat$msg <- dat$msg[dat$msg$time >= start & dat$msg$time <= stop, ]
  dat$samp <- dat$samp[dat$samp$time >= start & dat$samp$time <= stop, ]
  dat$event <- dat$event[dat$event$time >= start & dat$event$time <= stop, ]
  
  return(dat)
  
}
