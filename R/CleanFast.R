
CleanFast <- function(dat, env = parent.frame(n = 2)) {
  
  # set labels
  boundary.label <- env$exp$setup$message$boundary
  prime.label <- env$exp$setup$message$prime
  target.label <- env$exp$setup$message$target
  
  # trial loop  
  for (trial in 1:length(dat$item)) {
    # trial <- 94
    
    # set up output slot
    dat$item[[trial]]$clean$fast <- list(trigger = 0, blink = 0, out = 0, 
                                         seq = 1, time = 0, hook = 0, 
                                         change.sac = 0, 
                                         pre.time = 0, pre.prime = 0,
                                         prime.time = 0, post.prime = 0, 
                                         fix.dur = 0, fix.target = 0,
                                         crit = 0)
    
    # variables
    tmp <- dat$item[[trial]]$all
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
    dat$item[[trial]]$meta$boundary <- dat$item[[trial]]$meta$stimmat$xs[min(dat$item[[trial]]$meta$stimmat$letternum[dat$item[[trial]]$meta$stimmat$wordnum  == dat$item[[trial]]$meta$target])]
    
    
    # cleaning criteria
    # ------------------
    
    # 1. boundary.trigger: trigger problems (skips rest) (critical)
    
    # retrieve fixations    
    fix <- dat$item[[trial]]$fix
    
    # a) check whether boundary has been triggered
    if (nrow(boundary) == 0) {
      dat$item[[trial]]$clean$fast$trigger <- 1
      dat$item[[trial]]$clean$fast$crit <- 1
      next
    }
    
    # b) check if boundary change occured before first fixation
    if (fix$start[1] > boundary$start) {
      dat$item[[trial]]$clean$fast$trigger <- 1
      dat$item[[trial]]$clean$fast$crit <- 1
      next
    }
    # NOTE: Not sure how this is even possible, but it happened once
    
    # c) check if there is a fixation after boundary change 
    if (fix$start[nrow(fix)] < boundary$start) {
      dat$item[[trial]]$clean$fast$trigger <- 1
      dat$item[[trial]]$clean$fast$crit <- 1
      next
    }
    # NOTE: This can happen if the response button pressed too early
    
    # 2. boundary.blink (critical): 
    # check for blinks before/after boundary saccade
    # NOTE: is this really necessary (blinks already in target check)
    
    # a) blinks
    blink.before <- tail(fix$blink[fix$start <= boundary$start], n = 1)
    
    blink.after <- 1
    test <- head(fix$blink[fix$start > boundary$start], n = 1)
    if (length(test) > 0) {
      if (test == 0) {
        blink.after <- 0  
      }
    }
    
    if (blink.before == 1 | blink.after == 1) {
      dat$item[[trial]]$clean$fast$blink <- 1
      dat$item[[trial]]$clean$fast$crit <- 1
    }
    
    # b) outlier
    out.before <- 1
    test <- tail(fix$type[fix$start <= boundary$start], n = 1)
    if (length(test) > 0) {
      if (test == "in") {
        out.before <- 0  
      }
    }
    
    out.after <- 1
    test <- head(fix$type[fix$start > boundary$start], n = 1)
    if (length(test) > 0) {
      if (test == "in") {
        out.after <- 0  
      }
    }
    
    if (out.before == 1 | out.after == 1) {
      dat$item[[trial]]$clean$fast$out <- 1
      dat$item[[trial]]$clean$fast$crit <- 1
    }
    
    # 3. remove trials with non-standard pattern
    # remove trials with non-standard pattern
    
    find1 <- paste(c("SAC", boundary.label, prime.label, "FIX", target.label), collapse = "")
    find2 <- paste(c("SAC", boundary.label, "FIX", prime.label, target.label), collapse = "")
    stack <- paste(tmp$msg, collapse = "")
    
    if (length(grep(find1, stack)) > 0) {
      dat$item[[trial]]$clean$fast$seq <- 
        paste("SAC", boundary.label, prime.label, "FIX", sep = "-")
    } else if (length(grep(find2, stack)) > 0) {
      dat$item[[trial]]$clean$fast$seq <- 
        paste("SAC", boundary.label, "FIX", prime.label, sep = "-")
    } else {
      dat$item[[trial]]$clean$fast$crit <- 1
    }
    
    # 4. fast.time (critical):
    # remove if display change occured after 10 ms in fixation (Slattery et al., 2011)
    if (pre.prime$msg == "FIX" & (prime$start - pre.prime$start) > 10) {
      dat$item[[trial]]$clean$fast$time <- 1
      dat$item[[trial]]$clean$boundary$crit <- 1
    }
    
    # 5. fast.hook (critical):
    # check for J-hooks 
    
    # retrieve fixation after boundary change
    for (j in tmp$num[tmp$msg == boundary.label]:nrow(tmp)) {
      if(tmp$msg[tmp$num[j]] == "FIX") {
        fix.after <- tmp[tmp$num[j], ]
        break
      }
    }
    if (fix.after$xs <= as.numeric(dat$item[[trial]]$meta$boundary)) {
      dat$item[[trial]]$clean$fast$hook <-  1
      dat$item[[trial]]$clean$fast$crit <-  1
    }   
    
    
    # times
    # ------
    
    # duration of change saccade
    if (pre.boundary$msg == "SAC") {
      dat$item[[trial]]$clean$fast$change.sac <- pre.boundary$stop - pre.boundary$start  
    }
    dat$item[[trial]]$clean$fast$change.sac[length(dat$item[[trial]]$clean$fast$change.sac) == 0] = -999
    dat$item[[trial]]$clean$fast$change.sac[dat$item[[trial]]$clean$fast$change.sac == 0] = -999
    
    # check if boundary saccade is too long
    if (dat$item[[trial]]$clean$fast$change.sac > 80) {
      dat$item[[trial]]$clean$fast$change.sac[dat$item[[trial]]$clean$fast$change.sac > 80] = -999
      dat$item[[trial]]$clean$fast$blink <- 1
    }
    # TODO: value as parameter?
    
    # time between saccade onset and boundary
    dat$item[[trial]]$clean$fast$pre.time <- 
      boundary$start - pre.boundary$start[pre.boundary$msg == "SAC"]
    dat$item[[trial]]$clean$fast$pre.time[length(dat$item[[trial]]$clean$fast$pre.time) == 0] = -999
    dat$item[[trial]]$clean$fast$pre.time[dat$item[[trial]]$clean$fast$seq == 1] = -999
    
    # time between boundary and prime
    dat$item[[trial]]$clean$fast$pre.prime <- 
      prime$start - boundary$start
    dat$item[[trial]]$clean$fast$pre.prime[length(dat$item[[trial]]$clean$fast$pre.prime) == 0] = -999
    dat$item[[trial]]$clean$fast$pre.prime[dat$item[[trial]]$clean$fast$seq == 1] = -999
    
    # time between prime and target
    dat$item[[trial]]$clean$fast$prime.time <- 
      target$start - prime$start
    dat$item[[trial]]$clean$fast$prime.time[length(dat$item[[trial]]$clean$fast$prime.time) == 0] = -999
    dat$item[[trial]]$clean$fast$prime.time[dat$item[[trial]]$clean$fast$seq == 1] = -999
    
    # time between prime and fixation onset (negative if prime occured in fixation)
    if (post.prime$msg == "FIX") {
      dat$item[[trial]]$clean$fast$post.prime <-
        post.prime$start - prime$start
    } else if (pre.prime$msg == "FIX") {
      dat$item[[trial]]$clean$fast$post.prime <-
        pre.prime$start - prime$start 
    }
    if (dat$item[[trial]]$clean$fast$post.prime < -10) {
      dat$item[[trial]]$clean$fast$time <- 1
      dat$item[[trial]]$clean$fast$crit <- 1
    }
    dat$item[[trial]]$clean$fast$post.prime[length(dat$item[[trial]]$clean$fast$post.prime) == 0] = -999
    dat$item[[trial]]$clean$fast$post.prime[dat$item[[trial]]$clean$fast$seq == 1] = -999
    
    
    # duration of target fixation
    if (pre.target$msg == "FIX") {
      dat$item[[trial]]$clean$fast$fix.dur <- pre.target$stop - pre.target$start  
    } else if (pre.prime$msg == "FIX") {
      dat$item[[trial]]$clean$fast$fix.dur <- pre.prime$stop - pre.prime$start  
    }
    dat$item[[trial]]$clean$fast$fix.dur[length(dat$item[[trial]]$clean$fast$fix.dur) == 0] = -999
    dat$item[[trial]]$clean$fast$fix.dur[dat$item[[trial]]$clean$fast$seq == 1] = -999
    
    # time between fixation and target onset
    if (pre.target$msg == "FIX") {
      dat$item[[trial]]$clean$fast$fix.target <- target$start - pre.target$start
    } else if (pre.prime$msg == "FIX") {
      dat$item[[trial]]$clean$fast$fix.target <- pre.prime$start - target$start  
    }
    dat$item[[trial]]$clean$fast$fix.target[length(dat$item[[trial]]$clean$fast$fix.target) == 0] = -999
    dat$item[[trial]]$clean$fast$fix.target[dat$item[[trial]]$clean$fast$seq == 1] = -999
    
    # print(trial)
    
  }
  
  return(dat)
  
}
