
CleanOutlier <- function(dat, trial) {
  
  out <- dat$trial[[trial]]$fix
  change <- 0
  stop <- 0
  
  
  # rules
  # ------
  
  while (stop != 1) {
    
    # first -> delete 
    if (nrow(out) >= 1) {
      if (out$type[out$num == 1] == "out") {
        out <- out[out$num != 1, ]
        change <- 1
      }
    }
    
    # second -> delete first and second
    if (nrow(out) >= 2) {
      if (out$type[out$num == 2] == "out") {
        out <- out[out$num != 1 & out$num != 2, ]
        change <- 1
      }
    }
    
    # third -> delete first to third
    if (nrow(out) >= 3) {
      if (out$type[out$num == 3] == "out") {
        out <- out[out$num != 1 & out$num != 2 & out$num != 3, ]
        change <- 1
      }
    }
    
    # n -> delete n
    if (nrow(out) >= 1) {
      if (out$type[out$num == max(out$num)] == "out") {
        out <- out[out$num != max(out$num), ]
        change <- 1
      }
    }
    
    # n-1 -> delete n-1 and n
    if (nrow(out) >= 2) {
      if (out$type[out$num == max(out$num) - 1] == "out") {
        out <- out[out$num != max(out$num) & (out$num != max(out$num) - 1), ]
        change <- 1
      }
    }
    
    # n-2 -> delete n-2 to n
    if (nrow(out) >= 3) {
      if (out$type[out$num == max(out$num) - 2] == "out") {
        out <- out[out$num != max(out$num) & (out$num != max(out$num) - 1) & (out$num != max(out$num) - 2), ]
        change <- 1
      }
    }
    
    # repeat criterion
    stop <- 1
    if (change == 1) {
      if (nrow(out) > 0) {
        out$num <- 1:nrow(out)
        stop <- 0
        change <- 0
        if (sum(out$type == "out") == nrow(out)){
          stop <- 1
        }
      } else {
        stop <- 1
      }
    }
  }
  
  if (sum(out$type == "in") > 1) {
    dat$trial[[trial]]$fix <- out
  } else {
    dat$trial[[trial]]$fix <- NULL
  }
  
  return(dat)
  
}
