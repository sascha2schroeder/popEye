
# NOTE: Be careful. Variables also have to be changed in CreateOutput and CreateClean

CleanTarget <- function(dat, env = parent.frame(n = 2)) {
  
  for (trial in 1:length(dat$item)) {
    # trial <- 42
    
    
    # prep
    # ----
    
    target.ia <- dat$item[[trial]]$meta$target
    
    # set up output slot
    dat$item[[trial]]$clean$target <- list(blink = 0, 
                                            out = 0,
                                            first = 0,
                                            pre.blink = 0,
                                            pre.out = 0, 
                                            # pre.sac = 0,
                                            pre.launch = 0,
                                            pre.refix = 0, 
                                            pre.reg = 0,
                                            post.fix = 0,
                                            post.reg = 0,
                                            crit = 0)
    
    # select data
    tmp <- dat$item[[trial]]$fix
    tmpin <- tmp[tmp$type == "in", ]
    
    target.min <- tmpin$ianum[tmpin$ianum >= target.ia][1]
    
    # target start
    target.start <- tmp[is.na(tmp$ianum) == F &
                          tmp$ianum == target.min &
                          # tmp$ianum == target.ia &
                          tmp$ia.run == 1 &
                          tmp$ia.run.fix == 1, ]
    
    # target end
    target.end <- tail(tmp[is.na(tmp$ianum) == F &
                        tmp$ianum == target.min &
                        # tmp$ianum == target.ia &
                        tmp$ia.run == 1, ], n = 1)
    
    
    # general
    # --------
    
    target.range <- seq(from = min(dat$item[[trial]]$meta$stimmat$xs[dat$item[[trial]]$meta$stimmat$ianum == target.ia]),
                        to = max(dat$item[[trial]]$meta$stimmat$xe[dat$item[[trial]]$meta$stimmat$ianum == target.ia]))
    
    # target.post.fix: check whether there is a fixation on or after target IA
    # NOTE: this case is slightly different from the case below, but it is not worth to distinguish between them
    if (length(tmpin$ianum[tmpin$ianum >= target.ia]) == 0) {
      dat$item[[trial]]$clean$target$post.fix <- 1
      dat$item[[trial]]$clean$target$crit <- 1
      next  
    }
    
    # blinks
    blink <- dat$item[[trial]]$sac[dat$item[[trial]]$sac$msg == "BLINK", ]
    
    if (nrow(blink) > 0) {
      
      for (i in 1:nrow(blink)) {
        # i <- 1
        
        blink.range <- seq(from = blink$xs[i], to = blink$xe[i])
        
        # target.pre.blink: blink before target word (time)
        if (blink$start[i] < target.start$start) {
          dat$item[[trial]]$clean$target$pre.blink <- 1
        }
        
        # # target.pre.blink: blink before target word (space)
        # if (blink.range[1] < min(target.range)) {
        #   dat$item[[trial]]$clean$target$pre.blink <- 1
        # }
        
        # target.blink: blink overlaps target
        if (sum(target.range %in% blink.range) > 0) {
          dat$item[[trial]]$clean$target$blink <- 1
          dat$item[[trial]]$clean$target$crit <- 1
        }
        
        # print(i)
      }
      
    }
    
    # target.blink: check whether there is a blink directly before/after target fixation (critical)
    if (target.start$blink == 1 | target.end$blink == 1) {
      dat$item[[trial]]$clean$target$blink <- 1
      dat$item[[trial]]$clean$target$crit <- 1
    }
    
    # outlier
    if (length(tmp$fixid[tmp$type == "out"]) > 0) {
      
      # target.pre.out: check whether there is an outlying fixation before target fixation
      if (tmp$fixid[tmp$type == "out"] < target.start$fixid) {
        dat$item[[trial]]$clean$target$pre.out <- 1
        dat$item[[trial]]$clean$target$crit <- 1
      }
      
      # target.out: check whether there is an outlying fixation directly before or after target fixation
      if (tmp$fixid[tmp$type == "out"] == target.end$fixid - 1) {
        dat$item[[trial]]$clean$target$out <- 1
        dat$item[[trial]]$clean$target$crit <- 1
      }
      if (tmp$fixid[tmp$type == "out"] == target.end$fixid + 1) {
        dat$item[[trial]]$clean$target$out <- 1
        dat$item[[trial]]$clean$target$crit <- 1
      }
    }
    
    # target.first: check whether first fixation is on target or higher IA (critical)
    if (tmpin$ianum[1] >= target.ia) {
        dat$item[[trial]]$clean$target$first <- 1
        dat$item[[trial]]$clean$target$crit <- 1
        next
    }
    
    
    # pre-target behavior
    # --------------------
    
    # # target.pre.sac: check whether higher IA has been visited before
    # if (sum(tmp$ianum[1:target.start$fixid] > target.ia) > 0) {
    #   dat$item[[trial]]$clean$target$pre.sac <- 1
    # }
    # # NOTE: redundant with skip and firstskip -> delete?
    
    # pre.launch: saccade on target IA did not start on n-1 or n-2
    test <- tail(tmpin$ianum[tmpin$start < target.start$start], n = 1)
    if (length(test) == 0) {
      dat$item[[trial]]$clean$target$pre.launch <- 1
    } else {
      if (test < (target.ia - 2)) {
          dat$item[[trial]]$clean$target$pre.launch <- 1
      }
    }
    # NOTE: can also be used later (sac.in)
    # NOTE: needed?
    
    # target.pre.refix: check whether there is a refixation before target
    if (tmpin$ia.refix[tmpin$fixid == (target.start$fixid - 1)] == 1) {
      dat$item[[trial]]$clean$target$pre.refix <- 1 
    }
    # target.pre.refix: check whether there is a refixation before the target word,
    # that has been entered from the right
    # if (tmpin$ia.refix[tmpin$fixid == (target.start$fixid - 1)] == 1 & tmpin$sac.in[tmpin$fixid == (target.start$fixid - 1)] < 0) {
    #       dat$item[[trial]]$clean$target$pre.refix <- 1
    # }
    
    # NOTE: If the word of the pre-target fixation has been refixated, preview effects
    # can be different. This is particularly important when the first fixation was 
    # nearer to the target word and it got more preview than expected from its launch site
    # NOTE: A decision has to be made between the two definitions.
    
    # target.pre.reg: check whether higher letter than launch letter has been visited before
    let.hist = tmpin$letternum[tmpin$start < target.start$start]
    test = sum(let.hist[length(let.hist)] < let.hist[-length(let.hist)], na.rm = T)
    if (is.na(test) | test > 0) {
      dat$item[[trial]]$clean$target$pre.reg <- 1
    }
    
    
    # post-target behavior
    # ---------------------
    
    # target.post.fix: check whether there is a fixation after target IA
    if (length(tmpin$fixid[tmpin$fixid > target.end$fixid]) == 0) {
      dat$item[[trial]]$clean$target$post.fix <- 1
      dat$item[[trial]]$clean$target$crit <- 1
      next
    }
    # NOTE: needed?
    
    # # post.fix: check whether there is an outlier directly after target IA
    # test <- tmp[is.na(tmp$ianum) == F & tmp$ianum == target.min & tmp$ia.run == 1, ]
    # if (is.na(head(tmp$ianum[tmp$fixid > test$fixid[nrow(test)]], n = 1)) == T) {
    #   dat$item[[trial]]$clean$target$post.fix <- 1
    #   dat$item[[trial]]$clean$target$crit <- 1
    #   next
    # }
    
    # post.fix: check whether there is a fixation on an IA after target IA
    if (sum(tmpin$ianum[tmpin$fixid >= target.end$fixid] > target.ia) == 0)  {
      dat$item[[trial]]$clean$target$post.fix <- 1
      dat$item[[trial]]$clean$target$crit <- 1
      next
    }
    
    # post.reg: check whether last fixation on target is forward-oriented
    if (target.end$sac.out < 0) {
      dat$item[[trial]]$clean$target$post.reg <- 1
    }
    
  }
  
  return(dat)
  
}
