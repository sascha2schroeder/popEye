
CleanFast <- function(dat, env = parent.frame(n = 2)) {
  
  # TODO: generate problem report
  
  # set labels
  boundary.label <- env$exp$setup$message$boundary
  prime.label <- env$exp$setup$message$prime
  target.label <- env$exp$setup$message$target
  
  # trial loop  
  for (trial in 1:length(dat$trial)) {
    # trial <- 94
    
    # set up output slot
    dat$trial[[trial]]$clean$fast <- list(trigger = 0, blink = 0, seq = 0, 
                                              pattern = 0, time = 0, hook = 0, 
                                              crit = 0, sac.dur = 0, 
                                              pre.time = 0, pre.prime = 0,
                                              prime.time = 0, post.prime = 0, 
                                              fix.dur = 0, fix.target = 0)
    
    # variables
    tmp <- dat$trial[[trial]]$all
    boundary <- tmp[tmp$msg == boundary.label, ]
    pre.boundary <- tmp[tmp$num[tmp$msg == boundary.label] - 1, ]
    post.boundary <- tmp[tmp$num[tmp$msg == boundary.label] + 1, ]
    prime <- tmp[tmp$msg == prime.label, ]
    pre.prime <- tmp[tmp$num[tmp$msg == prime.label] - 1, ]
    post.prime <- tmp[tmp$num[tmp$msg == prime.label] + 1, ]
    target <- tmp[tmp$msg == target.label, ]
    pre.target <- tmp[tmp$num[tmp$msg == target.label] - 1, ]
    post.target <- tmp[tmp$num[tmp$msg == target.label] + 1, ]
    
    # compute boundary position
    dat$trial[[trial]]$meta$boundary <- dat$trial[[trial]]$meta$letter.boundary[dat$trial[[trial]]$meta$ia.boundary[dat$trial[[trial]]$meta$target]]
    
    # cleaning criteria
    # ------------------
    
    # 0. trigger problems (skips rest)
    
    # retrieve fixations    
    fix <- dat$trial[[trial]]$fix
    
    # check if boundary has been triggered
    if (nrow(boundary) == 0) {
      dat$trial[[trial]]$clean$fast$trigger <- 1
      dat$trial[[trial]]$clean$fast$crit <- 1
      next
    }

    # check if boundary change occured before first fixation
    # (not sure how this is possible, but it happened once)
    if (fix$start[1] > boundary$start) {
      dat$trial[[trial]]$clean$fast$trigger <- 1
      dat$trial[[trial]]$clean$fast$crit <- 1
      next
    }
    
    # check if there is a fixation after boundary change 
    # (response button pressed too early)
    
    if (fix$start[nrow(fix)] < boundary$start) {
      dat$trial[[trial]]$clean$fast$trigger <- 1
      dat$trial[[trial]]$clean$fast$crit <- 1
      next
    }
    
    
    # 1. check for blinks before/after boundary saccade
    blink.before <- tail(fix$blink[fix$start < boundary$start], n = 1)
    blink.after <- head(fix$blink[fix$start > boundary$start], n = 1)
    
    if (blink.before == 1 | blink.after == 1) {
      dat$trial[[trial]]$clean$fast$blink <- 1
    }
    
    # 2. remove trials with non-standard pattern
    
    find1 <- paste(c("SAC", boundary.label, prime.label, "FIX", target.label), collapse = "")
    find2 <- paste(c("SAC", boundary.label, "FIX", prime.label, target.label), collapse = "")
    stack <- paste(tmp$msg, collapse = "")
    
    if (length(grep(find1, stack)) > 0) {
      dat$trial[[trial]]$clean$fast$seq <- 
        paste(boundary.label, prime.label, "FIX", sep = "-")
    } else if (length(grep(find2, stack)) > 0) {
      dat$trial[[trial]]$clean$fast$seq <- 
        paste(boundary.label, "FIX", prime.label, sep = "-")
    } else {
      dat$trial[[trial]]$clean$fast$seq <- 0
    }
    
    if (dat$trial[[trial]]$clean$fast$seq == 0) {
      dat$trial[[trial]]$clean$fast$pattern <- 1
      dat$trial[[trial]]$clean$fast$seq <- paste(pre.prime$msg, 
                                                     prime$msg, post.prime$msg, sep = "-")
    }
    
    # 3. remove if display change occured after 10 ms in fixation (Slattery et al., 2011)
    
    if (pre.prime$msg == "FIX" & (prime$start - pre.prime$start) > 10) {
      dat$trial[[trial]]$clean$fast$time <- 1
    }
    
    # 4. check for J-hooks 
    
    # retrieve fixation after boundary change
    for (j in tmp$num[tmp$msg == boundary.label]:nrow(tmp)) {
      if(tmp$msg[tmp$num[j]] == "FIX") {
        fix.after <- tmp[tmp$num[j], ]
        break
      }
    }
    
    if (fix.after$xs <= as.numeric(dat$trial[[trial]]$meta$boundary)) {
      dat$trial[[trial]]$clean$fast$hook <-  1
    }   
    
    # combine
    if (sum(c(dat$trial[[trial]]$clean$fast$trigger, 
              dat$trial[[trial]]$clean$fast$blink, 
              dat$trial[[trial]]$clean$fast$pattern,
              dat$trial[[trial]]$clean$fast$time, 
              dat$trial[[trial]]$clean$fast$hook)) > 0) {
      dat$trial[[trial]]$clean$fast$crit <- 1   
    }
    
    
    # times
    # ------
    
    # duration of change saccade
    if (pre.boundary$msg == "SAC") {
      dat$trial[[trial]]$clean$fast$sac.dur <- pre.boundary$stop - pre.boundary$start  
    }
    dat$trial[[trial]]$clean$fast$sac.dur[length(dat$trial[[trial]]$clean$fast$sac.dur) == 0] = -999
    
    # time between saccade onset and boundary
    dat$trial[[trial]]$clean$fast$pre.time <- 
      boundary$start - pre.boundary$start[pre.boundary$msg == "SAC"]
    dat$trial[[trial]]$clean$fast$pre.time[length(dat$trial[[trial]]$clean$fast$pre.time) == 0] = -999
    
    # time between boundary and prime
    dat$trial[[trial]]$clean$fast$pre.prime <- 
      prime$start - boundary$start
    dat$trial[[trial]]$clean$fast$pre.prime[length(dat$trial[[trial]]$clean$fast$pre.prime) == 0] = -999
    
    # time between prime and target
    dat$trial[[trial]]$clean$fast$prime.time <- 
      target$start - prime$start
    dat$trial[[trial]]$clean$fast$prime.time[length(dat$trial[[trial]]$clean$fast$prime.time) == 0] = -999
    
    # time between prime and fixation onset (negative if prime occured in fixation)
    if (post.prime$msg == "FIX") {
      dat$trial[[trial]]$clean$fast$post.prime <-
        post.prime$start - prime$start
    } else if (pre.prime$msg == "FIX") {
      dat$trial[[trial]]$clean$fast$post.prime <-
        pre.prime$start - prime$start 
    }
    dat$trial[[trial]]$clean$fast$pre.prime[length(dat$trial[[trial]]$clean$fast$pre.prime) == 0] = -999
    
    # duration of target fixation
    if (pre.target$msg == "FIX") {
      dat$trial[[trial]]$clean$fast$fix.dur <- pre.target$stop - pre.target$start  
    } else if (pre.prime$msg == "FIX") {
      dat$trial[[trial]]$clean$fast$fix.dur <- pre.prime$stop - pre.prime$start  
    }
    dat$trial[[trial]]$clean$fast$fix.dur[length(dat$trial[[trial]]$clean$fast$fix.dur) == 0] = -999
    
    # time between fixation and target onset
    if (pre.target$msg == "FIX") {
      dat$trial[[trial]]$clean$fast$fix.target <- pre.target$start - target$start  
    } else if (pre.prime$msg == "FIX") {
      dat$trial[[trial]]$clean$fast$fix.target <- pre.prime$start - target$start  
    }
    dat$trial[[trial]]$clean$fast$fix.target[length(dat$trial[[trial]]$clean$fast$fix.target) == 0] = -999
    
    # print(trial)
    
  }
  
  return(dat)
  
}
