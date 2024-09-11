
# NOTE: assumes that all trials have the same start and stop message

RemoveSamples <- function(dat, env = parent.frame(n = 2)) {
  
  msg <- data.frame(matrix(data = NA, nrow = 1, ncol = 7)) 
  names(msg) <- c('trialid', 'trialnum', 'itemid', 'condition', 'dependency', 'time',  'msg')
 
  samp <- data.frame(matrix(data = NA, nrow = 1, ncol = 4)) 
  names(samp) <- c('time', 'x',  'y', 'pupil')
  
  event <- data.frame(matrix(data = NA, nrow = 1, ncol = 10)) 
  names(event) <- c('time',  'eye', 'msg', 'xs', 'ys', 'xe', 'ye', 'ps', 'amp', 'pv')

  for (trial in 1:max(dat$msg$trialnum)) {
    # trial = 1
    
    # timestamps
    start <- dat$msg$time[dat$msg$trialnum == trial & dat$msg$msg == env$exp$setup$message$start]
    stop <- dat$msg$time[dat$msg$trialnum == trial & dat$msg$msg == env$exp$setup$message$stop]
    
    # select data
    tmpsamp <- dat$samp[dat$samp$time >= start & dat$samp$time <= stop, ]
    tmpmsg <- dat$msg[dat$msg$time>= start & dat$msg$time <= stop, ] 
    tmpevent <- dat$event[dat$event$time>= start & dat$event$time <= stop, ] 
    
    # add to output
    samp <- rbind(samp, tmpsamp)
    msg <- rbind(msg, tmpmsg)
    event <- rbind(event, tmpevent)
    
  }
  
  dat$msg <- msg[-1, ]
  dat$samp <- samp[-1, ]
  dat$event <- event[-1, ]
  dat$event <- dat$event[is.na(dat$event) == F, ]
  
  return(dat)
  
}
