
CleanBoundary <- function(dat, env = parent.frame(n = 2)) {
  
  # TODO: generate problem report
  
  # set labels
  boundary.label <- env$exp$setup$message$boundary
  target.label <- env$exp$setup$message$target
  
  # trial loop  
  for (trial in 1:length(dat$trial)) {
  # for (trial in 94:95) {
    # trial <- 2
    
    # set up output slot
    dat$trial[[trial]]$clean$boundary <- list(trigger = 0, blink = 0, seq = 0, 
                                              pattern = 0, time = 0, hook = 0, 
                                              change.sac = 0, 
                                              pre.time = 0, target.time = 0, 
                                              post.time = 0, target.fix = 0,
                                              crit = 0)
    
    # variables
    tmp <- dat$trial[[trial]]$all
    boundary <- tmp[tmp$msg == boundary.label, ]
    pre.boundary <- tmp[tmp$num[tmp$msg == boundary.label] - 1, ]
    post.boundary <- tmp[tmp$num[tmp$msg == boundary.label] + 1, ]
    target <- tmp[tmp$msg == target.label, ]
    pre.target <- tmp[tmp$num[tmp$msg == target.label] - 1, ]
    post.target <- tmp[tmp$num[tmp$msg == target.label] + 1, ]
    
    # compute boundary position
    dat$trial[[trial]]$meta$boundary <- dat$trial[[trial]]$meta$stimmat$xs[min(dat$trial[[trial]]$meta$stimmat$letternum[dat$trial[[trial]]$meta$stimmat$word == dat$trial[[trial]]$meta$target])]
                                              
                                                                               
    # cleaning criteria
    # ------------------
    
    # 0. boundary.trigger: trigger problems (skips rest) (critical)
    
    # retrieve fixations    
    fix <- dat$trial[[trial]]$fix
    
    # a) check whether boundary has been triggered
    # check if boundary has been triggered (critical)
    if (nrow(boundary) == 0) {
      dat$trial[[trial]]$clean$boundary$trigger <- 1
      dat$trial[[trial]]$clean$boundary$crit <- 1
      next
    }

    # b) check if boundary change occured before first fixation
    # (not sure how this is even possible, but it happened once)
    if (fix$start[1] > boundary$start) {
      dat$trial[[trial]]$clean$boundary$trigger <- 1
      dat$trial[[trial]]$clean$boundary$crit <- 1
      next
    }
    
    # c) check if there is a fixation after boundary change 
    # (response button pressed too early)
    
    if (fix$start[nrow(fix)] < boundary$start) {
      dat$trial[[trial]]$clean$boundary$trigger <- 1
      dat$trial[[trial]]$clean$boundary$crit <- 1
      next
    }

    
    # 1. boundary.blink (critical): 
    # check for blinks before/after boundary saccade
    
    blink.before <- tail(fix$blink[fix$start <= boundary$start], n = 1)
    
    blink.after <- 1
    test <- head(fix$blink[fix$start > boundary$start], n = 1)
    if (length(test) > 0) {
      if (test == 0) {
        blink.after <- 0  
      }
    }
    
    if (blink.before == 1 | blink.after == 1) {
      dat$trial[[trial]]$clean$boundary$blink <- 1
    }
    
    
    # 2. boundary.pattern (critical): 
    # remove trials with non-standard pattern
    
    find1 <- paste(c("SAC", boundary.label, target.label, "FIX"), collapse = "")
    find2 <- paste(c("SAC", boundary.label, "FIX", target.label), collapse = "")
    stack <- paste(tmp$msg, collapse = "")
    
    if (length(grep(find1, stack)) > 0) {
      dat$trial[[trial]]$clean$boundary$seq <- 
        paste(boundary.label, target.label, "FIX", sep = "-")
    } else if (length(grep(find2, stack)) > 0) {
      dat$trial[[trial]]$clean$boundary$seq <- 
        paste(boundary.label, "FIX", target.label, sep = "-")
    } else {
      dat$trial[[trial]]$clean$boundary$seq <- 0
    }
    
    if (dat$trial[[trial]]$clean$boundary$seq == 0) {
      dat$trial[[trial]]$clean$boundary$pattern <- 1
      dat$trial[[trial]]$clean$boundary$seq <- paste(pre.target$msg, 
                                                     target$msg, post.target$msg, sep = "-")
    }
    
    # 3. boundary.time (critical):
    # remove if display change occured after 10 ms in fixation (Slattery et al., 2011)
    
    if (pre.target$msg == "FIX" & (target$start - pre.target$start) > 10) {
      dat$trial[[trial]]$clean$boundary$time <- 1
    }
    
    # 4. boundary.hook (critical):
    # check for J-hooks 
    
    # retrieve fixation after boundary change
    for (j in tmp$num[tmp$msg == boundary.label]:nrow(tmp)) {
      if(tmp$msg[tmp$num[j]] == "FIX") {
        fix.after <- tmp[tmp$num[j], ]
        break
      }
    }
    
    # TODO: change slot (to boundary sub-slot)
    if (fix.after$xs <= as.numeric(dat$trial[[trial]]$meta$boundary)) {
      dat$trial[[trial]]$clean$boundary$hook <-  1
    }   

        
    # times
    # ------
    
    # duration of change saccade
    if (pre.boundary$msg == "SAC") {
      dat$trial[[trial]]$clean$boundary$change.sac <- pre.boundary$stop - pre.boundary$start  
    }
    dat$trial[[trial]]$clean$boundary$change.sac[length(dat$trial[[trial]]$clean$boundary$change.sac) == 0] = -999
    
    # check if boundary saccade is too long
    if (dat$trial[[trial]]$clean$boundary$change.sac > 80) {
      dat$trial[[trial]]$clean$boundary$blink <- 1
    }
    # TODO: include parameter in call function
    
    # time between saccade onset and boundary
    dat$trial[[trial]]$clean$boundary$pre.time <- 
      boundary$start - pre.boundary$start[pre.boundary$msg == "SAC"]
    dat$trial[[trial]]$clean$boundary$pre.time[length(dat$trial[[trial]]$clean$boundary$pre.time) == 0] = -999
    
    # time between boundary and target
    dat$trial[[trial]]$clean$boundary$target.time <- 
      min(target$start - boundary$start)
    dat$trial[[trial]]$clean$boundary$target.time[length(dat$trial[[trial]]$clean$boundary$target.time) == 0] = -999
    
    # time between target and fixation onset (negative if target occured in fixation)
    if (pre.target$msg == "FIX") {
      dat$trial[[trial]]$clean$boundary$post.time <-
        min(pre.target$start - target$start, na.rm = T)
    } else {
      dat$trial[[trial]]$clean$boundary$post.time <-
        min(post.target$start - target$start, na.rm = T)
    }
    dat$trial[[trial]]$clean$boundary$post.time[length(dat$trial[[trial]]$clean$boundary$post.time) == 0] = -999
    
    # duration of fixation after change
    if (pre.target$msg == "FIX") {
      dat$trial[[trial]]$clean$boundary$target.fix <- min(pre.target$stop - pre.target$start, na.rm = T) 
    } else {
      dat$trial[[trial]]$clean$boundary$target.fix <- min(post.target$stop - post.target$start, na.rm = T) 
    }
    
    
    # combine
    # --------
    
    if (sum(c(dat$trial[[trial]]$clean$boundary$trigger, 
              dat$trial[[trial]]$clean$boundary$blink, 
              dat$trial[[trial]]$clean$boundary$pattern,
              dat$trial[[trial]]$clean$boundary$time, 
              dat$trial[[trial]]$clean$boundary$hook)) > 0) {
      dat$trial[[trial]]$clean$boundary$crit <- 1   
    }
    
    # print(trial)
    
  }
  
  return(dat)
  
}
