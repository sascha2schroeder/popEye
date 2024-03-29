
# NOTE: Be careful. Variables also have to be changed in CreateOutput and CreateClean

CleanBoundary <- function(dat, env = parent.frame(n = 2)) {
  
  # NOTE: generate problem report
  
  # set labels
  boundary.label <- env$exp$setup$message$boundary
  target.label <- env$exp$setup$message$target
  
  # trial loop  
  for (trial in 1:length(dat$item)) {
  # trial <- 2
    
    # set up output slot
    dat$item[[trial]]$clean$boundary <- list(trigger = 0, blink = 0, out = 0,
                                              seq = 1, time = 0, hook = 0, 
                                              change.sac = 0, 
                                              pre.time = 0, target.time = 0, 
                                              post.time = 0, target.fix = 0,
                                              crit = 0)
    
    # variables
    tmp <- dat$item[[trial]]$all
    boundary <- tmp[tmp$msg == boundary.label, ]
    pre.boundary <- tmp[tmp$num[tmp$msg == boundary.label] - 1, ]
    post.boundary <- tmp[tmp$num[tmp$msg == boundary.label] + 1, ]
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
      dat$item[[trial]]$clean$boundary$trigger <- 1
      dat$item[[trial]]$clean$boundary$crit <- 1
      next
    }

    # b) check if boundary change occured before first fixation
    if (fix$start[1] > boundary$start) {
      dat$item[[trial]]$clean$boundary$trigger <- 1
      dat$item[[trial]]$clean$boundary$crit <- 1
      next
    }
    # NOTE: Not sure how this is even possible, but it happened once
    
    # c) check if there is a fixation after boundary change 
    if (fix$start[nrow(fix)] < boundary$start) {
      dat$item[[trial]]$clean$boundary$trigger <- 1
      dat$item[[trial]]$clean$boundary$crit <- 1
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
      dat$item[[trial]]$clean$boundary$blink <- 1
      dat$item[[trial]]$clean$boundary$crit <- 1
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
      dat$item[[trial]]$clean$boundary$out <- 1
      dat$item[[trial]]$clean$boundary$crit <- 1
    }
    
    
    # 3. boundary.pattern (critical): 
    # remove trials with non-standard pattern
    
    find1 <- paste(c("SAC", boundary.label, target.label, "FIX"), collapse = "")
    find2 <- paste(c("SAC", boundary.label, "FIX", target.label), collapse = "")
    find3 <- paste(c("FIX", boundary.label, target.label, "SAC"), collapse = "")
    stack <- paste(tmp$msg, collapse = "")
    
    if (length(grep(find1, stack)) > 0) {
      dat$item[[trial]]$clean$boundary$seq <- 
        paste(boundary.label, target.label, "FIX", sep = "-")
    } else if (length(grep(find2, stack)) > 0) {
      dat$item[[trial]]$clean$boundary$seq <- 
        paste(boundary.label, "FIX", target.label, sep = "-")
    } else if (length(grep(find3, stack)) > 0) {
      dat$item[[trial]]$clean$boundary$seq <- 
        paste("FIX", boundary.label, target.label, sep = "-")
    } else {
      dat$item[[trial]]$clean$boundary$crit <- 1
    }
    
    # 4. boundary.time (critical):
    # remove if display change occured after 10 ms in fixation (Slattery et al., 2011)
    if (pre.target$msg == "FIX" & (target$start - pre.target$start) > 10) {
      dat$item[[trial]]$clean$boundary$time <- 1
      dat$item[[trial]]$clean$boundary$crit <- 1
    }
    
    # 5. boundary.hook (critical):
    # check for J-hooks 
    
    # retrieve fixation after boundary change
    for (j in tmp$num[tmp$msg == boundary.label]:nrow(tmp)) {
      if(tmp$msg[tmp$num[j]] == "FIX") {
        fix.after <- tmp[tmp$num[j], ]
        break
      }
    }
    
    if (fix.after$xs <= as.numeric(dat$item[[trial]]$meta$boundary)) {
      dat$item[[trial]]$clean$boundary$hook <-  1
      dat$item[[trial]]$clean$boundary$crit <- 1
    }   

        
    # times
    # ------
    
    # duration of change saccade
    if (pre.boundary$msg == "SAC") {
      dat$item[[trial]]$clean$boundary$change.sac <- pre.boundary$stop - pre.boundary$start  
    }
    dat$item[[trial]]$clean$boundary$change.sac[length(dat$item[[trial]]$clean$boundary$change.sac) == 0] = -999
    dat$item[[trial]]$clean$boundary$change.sac[dat$item[[trial]]$clean$boundary$change.sac == 0] = -999
    
    # check if boundary saccade is too long
    if (dat$item[[trial]]$clean$boundary$change.sac > 80) {
      dat$item[[trial]]$clean$boundary$change.sac[dat$item[[trial]]$clean$boundary$change.sac > 80] = -999
      dat$item[[trial]]$clean$boundary$blink <- 1
    }
    # TODO: value as parameter?
    
    # time between saccade onset and boundary
    dat$item[[trial]]$clean$boundary$pre.time <- 
      boundary$start - pre.boundary$start[pre.boundary$msg == "SAC"]
    dat$item[[trial]]$clean$boundary$pre.time[length(dat$item[[trial]]$clean$boundary$pre.time) == 0] = -999
    dat$item[[trial]]$clean$boundary$pre.time[dat$item[[trial]]$clean$boundary$seq == 1] = -999
    
    # time between boundary and target
    dat$item[[trial]]$clean$boundary$target.time <- 
      min(target$start - boundary$start)
    dat$item[[trial]]$clean$boundary$target.time[length(dat$item[[trial]]$clean$boundary$target.time) == 0] = -999
    dat$item[[trial]]$clean$boundary$target.time[dat$item[[trial]]$clean$boundary$seq == 1] = -999
    
    # time between target and fixation onset (negative if target occured in fixation)
    if (pre.target$msg == "FIX") {
      dat$item[[trial]]$clean$boundary$post.time <-
        min(pre.target$start - target$start, na.rm = T)
    } else {
      dat$item[[trial]]$clean$boundary$post.time <-
        min(post.target$start - target$start, na.rm = T)
    }
    if (pre.boundary$msg == "FIX") {
      dat$item[[trial]]$clean$boundary$post.time <- 
        pre.boundary$start[pre.boundary$msg == "FIX"] - boundary$start
    }
    if (dat$item[[trial]]$clean$boundary$post.time <= -10) {
      dat$item[[trial]]$clean$boundary$time <- 1
      dat$item[[trial]]$clean$boundary$crit <- 1
    }
    dat$item[[trial]]$clean$boundary$post.time[length(dat$item[[trial]]$clean$boundary$post.time) == 0] = -999
    dat$item[[trial]]$clean$boundary$post.time[dat$item[[trial]]$clean$boundary$seq == 1] = -999
    
    # duration of fixation after change
    if (pre.target$msg == "FIX") {
      dat$item[[trial]]$clean$boundary$target.fix <- min(pre.target$stop - pre.target$start, na.rm = T) 
    } else {
      dat$item[[trial]]$clean$boundary$target.fix <- min(post.target$stop - post.target$start, na.rm = T) 
    }
    if (pre.boundary$msg == "FIX") {
      dat$item[[trial]]$clean$boundary$target.fix <- 
        pre.boundary$stop - target$start
    }
    dat$item[[trial]]$clean$boundary$target.fix[dat$item[[trial]]$clean$boundary$seq == 1] = -999
    
    # print(trial)
    
  }
  
  return(dat)
  
}
