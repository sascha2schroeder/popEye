
ComputeLineChange <- function(dat, trial) {
  
  # trial = 1
  
  dat$trial[[trial]]$fix$line.change <- NA
  dat$trial[[trial]]$fix$line.change[1] <- 0
  
  for (j in 2:nrow(dat$trial[[trial]]$fix)) {
    
    if (dat$trial[[trial]]$fix$line[j - 1] == 0 | dat$trial[[trial]]$fix$line[j] == 0) {
      next
    } else {
      dat$trial[[trial]]$fix$line.change[j] <- dat$trial[[trial]]$fix$line[j] - dat$trial[[trial]]$fix$line[j - 1] 
    }
    
    # # no line change
    # if (dat$trial[[trial]]$fix$line[j - 1] == dat$trial[[trial]]$fix$line[j]) {
    #   dat$trial[[trial]]$fix$line.change[j] <- 0
    # }
    # 
    # # line ahead
    # if (dat$trial[[trial]]$fix$line[j - 1] < dat$trial[[trial]]$fix$line[j]) {
    #   dat$trial[[trial]]$fix$line.change[j] <- 1
    # }
    #     
    # # line before
    # if (dat$trial[[trial]]$fix$line[j - 1] > dat$trial[[trial]]$fix$line[j]) {
    #   dat$trial[[trial]]$fix$sac.in[j] <- -1
    # }
    
  }
  
  return(dat)
  
}