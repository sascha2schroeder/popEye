
CleanTarget <- function(dat, env = parent.frame(n = 2)) {
  
  for (trial in 1:length(dat$trial)) {
    # trial <- 9
    
    
    # prep
    # ----
    
    target.ia <- dat$trial[[trial]]$meta$target
    
    # set up output slot
    dat$trial[[trial]]$clean$target <- list(blink = 0, 
                                            out = 0,
                                            first = 0,
                                            pre.sac = 0,
                                            pre.launch = 0,
                                            pre.refix = 0, 
                                            pre.reg = 0,
                                            post.fix = 0,
                                            post.sac = 0,
                                            post.reg = 0,
                                            crit = 0)
    
    # select data
    tmp <- dat$trial[[trial]]$fix
    
    target.min <- tmp$ianum[is.na(tmp$ianum) == F & tmp$ianum >= target.ia][1]
    
    # target start
    target.start <- tmp[is.na(tmp$ianum) == F &
                          tmp$ianum == target.min &
                          tmp$ia.run == 1 &
                          tmp$ia.run.fix == 1, ]
    
    # target end
    target.end <- tmp[is.na(tmp$ianum) == F &
                        tmp$ianum == target.min &
                        tmp$ia.run == 1, ]
    target.end <- target.end[nrow(target.end), ]
    
    
    # general
    # --------
    
    # NOTE: is this really necessary?
    target.range <- seq(from = min(dat$trial[[trial]]$meta$stimmat$xs[dat$trial[[trial]]$meta$stimmat$ianum == target.ia]),
                        to = max(dat$trial[[trial]]$meta$stimmat$xe[dat$trial[[trial]]$meta$stimmat$ianum == target.ia]))
    
    if (is.na(target.min) == F) {
      min.range <- seq(from = min(dat$trial[[trial]]$meta$stimmat$xs[dat$trial[[trial]]$meta$stimmat$ianum == target.min]),
                       to = max(dat$trial[[trial]]$meta$stimmat$xe[dat$trial[[trial]]$meta$stimmat$ianum == target.min]))
    } else {
      # target.post.fix: check whether there is a fixation after target IA
      dat$trial[[trial]]$clean$target$post.fix <- 1
      dat$trial[[trial]]$clean$target$crit <- 1
      next  
    }
    
    for (i in 1:nrow(dat$trial[[trial]]$sac)) {
      # i <- 1
      if (dat$trial[[trial]]$sac$msg[i] == "BLINK" & dat$trial[[trial]]$sac$start[i] < target.start$start) {
        blink.range <- seq(from = dat$trial[[trial]]$sac$xs[i], to = dat$trial[[trial]]$sac$xe[i])
        if (blink.range[1] < min(target.range) | blink.range[1] < min(min.range)) {
          dat$trial[[trial]]$clean$target$blink <- 1
          dat$trial[[trial]]$clean$target$crit <- 1
        }
      }
      # print(i)
    }
    
    # target.out: check whether there is an outlying fixation before target fixation
    if (length(tmp$fixid[tmp$type == "out"]) > 0 ) {
      if (tmp$fixid[tmp$type == "out"] < target.start$fixid) {
        dat$trial[[trial]]$clean$target$out <- 1
        dat$trial[[trial]]$clean$target$crit <- 1
      }
      if (tmp$fixid[tmp$type == "out"] == target.end$fixid + 1) {
        dat$trial[[trial]]$clean$target$out <- 1
        dat$trial[[trial]]$clean$target$crit <- 1
      }
    }
    
    # target.blink: check whether there is a blink directly before/after target fixation (critical)
    if (target.start$blink == 1 | target.end$blink == 1) {
      dat$trial[[trial]]$clean$target$blink <- 1
      dat$trial[[trial]]$clean$target$crit <- 1
    }
    
    # target.first: check whether first fixation is on target or higher IA (critical)
    if (is.na(tmp$ianum[1]) == T) {
      dat$trial[[trial]]$clean$target$first <- 1
      dat$trial[[trial]]$clean$target$crit <- 1
    } else {
      if (tmp$ianum[1] == target.ia | tmp$ianum[1] == target.min) {
        dat$trial[[trial]]$clean$target$first <- 1
        dat$trial[[trial]]$clean$target$crit <- 1
      }
    }
    
    
    # pre-target behavior
    # --------------------
    
    # target.pre.sac: check whether higher IA has been visited before
    if (target.min > target.ia) {
      dat$trial[[trial]]$clean$target$pre.sac <- 1
    }
    # NOTE: redundant with skip and firstskip -> delete?
    
    # pre.launch: saccade on target IA did not start on n-1 or n-2
    test <- tail(tmp$ianum[tmp$start < target.start$start], n = 1)
    if (length(test) == 0) {
      dat$trial[[trial]]$clean$target$pre.launch <- 1
    } else {
      if (is.na(test) == T) {
        dat$trial[[trial]]$clean$target$pre.launch <- 1
      } else {
        if (test < (target.ia - 2)) {
          dat$trial[[trial]]$clean$target$pre.launch <- 1
        }
      }
    }
    # NOTE: can also be used later (sac.in)
    # NOTE: needed?
    
    # pre.refix: check whether there is a refixation before target
    # check whether there is a fixation before target fixation
    if (length(tmp$ia.refix[tmp$fixid == (target.start$fixid - 1)]) == 0) {
      dat$trial[[trial]]$clean$target$pre.refix <- 1
    } else {
      # check whether fixation before target fixation has valid refix value
      if (is.na(tmp$ia.refix[tmp$fixid == (target.start$fixid - 1)]) == T) {
        dat$trial[[trial]]$clean$target$pre.refix <- 1
      } else {
        # if previous fixation is refixation and has been entered from the right
        if (tmp$ia.refix[tmp$fixid == (target.start$fixid - 1)] == 1 & tmp$sac.in[tmp$fixid == (target.start$fixid - 1)] < 0) {
          dat$trial[[trial]]$clean$target$pre.refix <- 1
        }
      }
    }
    # NOTE: can also be used later (sac.in/refixation on pre-target IA)
    # NOTE: not working when target.blink or target.out (?)
    
    # pre.reg: check whether higher letter than launch letter has been visited before
    let.hist = tmp$letternum[tmp$start < target.start$start]
    test = sum(let.hist[length(let.hist)] < let.hist[-length(let.hist)], na.rm = T)
    if (is.na(test) | test > 0) {
      dat$trial[[trial]]$clean$target$pre.reg <- 1
    }
    # NOTE: cannot be used later
    
    
    # post-target behavior
    # ---------------------
    
    # post.fix: check whether there is a fixation after target IA
    if (length(tmp$fixid[tmp$fixid > target.end$fixid]) == 0) {
      dat$trial[[trial]]$clean$target$post.fix <- 1
      dat$trial[[trial]]$clean$target$crit <- 1
      next
    }
    
    # post.fix: check whether there is an outlier directly after target IA
    test <- tmp[is.na(tmp$ianum) == F & tmp$ianum == target.min & tmp$ia.run == 1, ]
    if (is.na(head(tmp$ianum[tmp$fixid > test$fixid[nrow(test)]], n = 1)) == T) {
      dat$trial[[trial]]$clean$target$post.fix <- 1
      dat$trial[[trial]]$clean$target$crit <- 1
      next
    }
    
    # post.fix: check whether there is a fixation on an IA after target IA
    if (sum(tmp$ianum[is.na(tmp$ianum) == F & tmp$fixid >= target.end$fixid] > target.ia) == 0)  {
      dat$trial[[trial]]$clean$target$post.fix <- 1
      dat$trial[[trial]]$clean$target$crit <- 1
      next
    }
    
    # post.sac: check whether next fixation is a forward-fixation
    if (target.end$sac.out < 0) {
      dat$trial[[trial]]$clean$target$post.sac <- 1
    }
    # NOTE: useful?
    
    # post.reg: check whether target IA was left to the right
    if (is.na(tmp$sac.out[tmp$fixid == (target.end$fixid + 1)]) == T | tmp$sac.out[tmp$fixid == (target.end$fixid + 1)] < 0) {
      dat$trial[[trial]]$clean$target$post.reg <- 1
    }
    # NOTE: useful?
    
  }
  
  return(dat)
  
}
