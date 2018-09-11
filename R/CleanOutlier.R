
CleanOutlier <- function(dat, trial) {
  
  out <- dat$trial[[trial]]$fix
  change <- 0
  stop <- 0

  # rules
  # ------
  
  while (stop != 1) {
    
    # first -> delete 
    if (out$type[out$num == 1] == "out") {
      out <- out[out$num != 1, ]
      change <- 1
    }
    
    # second -> delete first and second
    if (out$type[out$num == 2] == "out") {
      out <- out[out$num != 1 & out$num != 2, ]
      change <- 1
    }
    
    # final -> delete 
    if (out$type[out$num == max(out$num)] == "out") {
      out <- out[out$num != max(out$num), ]
      change <- 1
    }
    
    # prefinal -> delete prefinal and final 
    if (out$type[out$num == max(out$num) - 1] == "out") {
      out <- out[out$num != max(out$num) & (out$num != max(out$num) - 1), ]
      change <- 1
    }
    
    # NOTE: experimental!
    
    # # n - 2
    # if (out$type[out$num == max(out$num) - 2] == "out" & out$ianum[out$num == max(out$num) - 2] == max(dat$trial[[trial]]$meta$stimmat$ia)) {
    #   out <- out[out$num != max(out$num) & (out$num != max(out$num) - 1) & (out$num != max(out$num) - 2)  & (out$num != max(out$num) - 3), ]
    #   change <- 1
    # }
    # 
    # # n - 3
    # if (out$type[out$num == max(out$num) - 3] == "out" & out$ianum[out$num == max(out$num) - 3] == max(dat$trial[[trial]]$meta$stimmat$ia)) {
    #   out <- out[out$num != max(out$num) & (out$num != max(out$num) - 1) & (out$num != max(out$num) - 2), ]
    #   change <- 1
    # }
    
    # repeat criterion
    stop <- 1
    if (change == 1) {
      out$num <- 1:nrow(out)
      stop <- 0
      change <- 0
      if (sum(out$type == "out") == nrow(out)){
        stop <- 1
      }
    }
  }
  
  dat$trial[[trial]]$fix <- out
  
  return(dat)
  
}
