
CleanTarget <- function(dat, env = parent.frame(n = 2)) {
    
  for (trial in 1:length(dat$trial)) {
    # trial <- 9
    
    target.word <- dat$trial[[trial]]$meta$target
    
    # set up output slot
    dat$trial[[trial]]$clean$target <- list(fix = 0, blink = 0, pre.sac = 0, 
                                            pre.skip = 0, pre.launch = 0, 
                                            pre.refix = 0, pre.reg = 0, 
                                            post.fix = 0, post.sac = 0,
                                            post.refix = 0, post.reg = 0, 
                                            crit = 0)
    
    # select data
    tmp <- dat$trial[[trial]]$fix
    
    # target start
    target.start <- tmp[is.na(tmp$ianum) == F &
                    tmp$ianum == target.word &
                    tmp$ia.run == 1 &
                    tmp$ia.run.fix == 1, ]
    
    # target end
    target.end <- tmp[is.na(tmp$ianum) == F & 
                    tmp$ianum == target.word & 
                    tmp$ia.run == 1, ]
    target.end <- target.end[nrow(target.end), ]
      
    # check whether blink involves target IA
    target.range <- seq(from = min(dat$trial[[trial]]$meta$stimmat$xs[dat$trial[[trial]]$meta$stimmat$ia == target.word]),
                        to = max(dat$trial[[trial]]$meta$stimmat$xe[dat$trial[[trial]]$meta$stimmat$ia == target.word]))
    
    for (i in 1:nrow(dat$trial[[trial]]$sac)) {
      # i <- 1
      if (dat$trial[[trial]]$sac$msg[i] == "BLINK") {
        blink.range <- seq(from = dat$trial[[trial]]$sac$xs[i], to = dat$trial[[trial]]$sac$xe[i])
        if (sum(blink.range %in% target.range) > 0) {
          dat$trial[[trial]]$clean$target$blink <- 1
        }
      }
      # print(i)
    }
    
    # check whether target word has been fixated
    if (nrow(target.start) == 0) {
      dat$trial[[trial]]$clean$target$fix <- 1
      dat$trial[[trial]]$clean$target$crit <- 1
      next
    }
    # NOTE: can also be used later (skip)
    

    if (nrow(target.start) > 0) {
      
      # blinks 
      # -------
      
      # blink before target fixation
      if (sum(dat$trial[[trial]]$all$msg[dat$trial[[trial]]$all$start < target.start$start] == "BLINK") > 0) {
        dat$trial[[trial]]$clean$target$blink <- 1
      }
      
      # blink directly after target fixation 
      if (head(dat$trial[[trial]]$all$msg[dat$trial[[trial]]$all$start > 
                                            target.start$start], n = 1) == "BLINK") {
        dat$trial[[trial]]$clean$target$blink <- 1
      }
      # NOTE: can also be used later (blink.after)
      
      
      # pre-target behavior
      # --------------------
      
      # target word entered from lower IA
      if (is.na(target.start$sac.in) == T | target.start$sac.in < 0) {
        dat$trial[[trial]]$clean$target$pre.sac <- 1
      }
      # NOTE: can also be used later (sac.in/firstskip)
      
      # check whether any higher IA has been visited before
      if (sum(tmp$word[tmp$num < target.start$num] > target.word) > 0) {
        dat$trial[[trial]]$clean$target$pre.skip <- 1
      }
      # NOTE: can also be used later (firstskip)

      # saccade on target word did not start on n-1 or n-2
      test <- tail(tmp$word[tmp$start < target.start$start], n = 1)
      if (length(test) == 0) {
        dat$trial[[trial]]$clean$target$pre.launch <- 1
      } else {
        if (test < (target.word - 2)) {
          dat$trial[[trial]]$clean$target$pre.launch <- 1  
        }
      } 
      # NOTE: can also be used later (sac.in)
      
      # check whether pretarget fixation is regressive refixation 
      if (length(tmp$refix[tmp$num == (target.start$num - 1)]) == 0) {
        dat$trial[[trial]]$clean$target$pre.refix <- 1
      } else {
        if (tmp$refix[tmp$num == (target.start$num - 1)] == 1 & tmp$sac.in[tmp$num == (target.start$num - 1)] < 0) {
          dat$trial[[trial]]$clean$target$pre.refix <- 1  
        }
      }
      # NOTE: can also be used later (sac.in/refixation on pre-target word)
      
      # check whether higher letter than launch letter has been visited before
      let.hist = tmp$letter[tmp$start < target.start$start]
      test = sum(let.hist[length(let.hist)] < let.hist[-length(let.hist)], na.rm = T)
      if (is.na(test) | test > 0) {
        dat$trial[[trial]]$clean$target$pre.reg <- 1
      }
      # NOTE: cannot be used later
      
                  
      # post-target behavior
      # ---------------------
      
      # check whether there is a fixation after target word
      if (length(tmp$num[tmp$num > target.end$num]) == 0) {
        dat$trial[[trial]]$clean$target$post.fix <- 1
        dat$trial[[trial]]$clean$target$crit <- 1
        next
      }
      
      # check whether there is an outlier directly after target word
      target.ia <- tmp[is.na(tmp$ianum) == F & tmp$ianum == target.word & tmp$ia.run == 1, ]
      if (is.na(head(tmp$ianum[tmp$num > target.ia$num[nrow(target.ia)]], n = 1)) == T) {
        dat$trial[[trial]]$clean$target$post.fix <- 1
        dat$trial[[trial]]$clean$target$crit <- 1
        next
      }
      
      # check whether there is a fixation on a word after target word
      if (sum(tmp$ianum[is.na(tmp$ianum) == F & tmp$num > target.end$num] > target.word) == 0)  {
        dat$trial[[trial]]$clean$target$post.fix <- 1
        dat$trial[[trial]]$clean$target$crit <- 1
        next
      }
      
      # check whether next fixation is a forward-fixation
      if (target.end$sac.out < 0) {
        dat$trial[[trial]]$clean$target$post.sac <- 1
      }
      
      # check whether next fixation is a regressive refixation
      if (tmp$ia.refix[tmp$num == (target.end$num + 1)] == 1 & tmp$sac.out[target.end$num] < 0)  {
        dat$trial[[trial]]$clean$target$post.refix <- 1
      }
      
      # check whether target word was left to the right (?)
      if (head(tmp$ianum[tmp$num > target.ia$num[nrow(target.ia)]], n = 1) < target.word) {
        dat$trial[[trial]]$clean$target$post.reg <- 1
      }
      
    }
    
    # combine
    if (sum(c(# dat$trial[[trial]]$clean$target$fix, 
              dat$trial[[trial]]$clean$target$blink,
              # dat$trial[[trial]]$clean$target$pre.sac, 
              # dat$trial[[trial]]$clean$target$pre.skip,
              # dat$trial[[trial]]$clean$target$pre.launch, 
              # dat$trial[[trial]]$clean$target$pre.refix,
              # dat$trial[[trial]]$clean$target$pre.reg,
              dat$trial[[trial]]$clean$target$post.fix
              # dat$trial[[trial]]$clean$target$post.sac,
              # dat$trial[[trial]]$clean$target$post.refix,
              # dat$trial[[trial]]$clean$target$post.reg
              )) > 0) {
      dat$trial[[trial]]$clean$target$crit = 1   
    }    
    
    # print(trial)
    
  }
  
  return(dat)
  
}
