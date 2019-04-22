
ComputeSaccadeLength <- function(dat, trial) {
  
  # trial = 1
  
  # NOTE: saccades from outlying fixations do not receive saccade lengths
  # NOTE: y distance is ignored in multi-line saccades
    
  # incoming
  dat$trial[[trial]]$fix$sac.in <- NA
  
  for (j in 2:nrow(dat$trial[[trial]]$fix)) {
    
    if (is.na(dat$trial[[trial]]$fix$line[j]) | is.na(dat$trial[[trial]]$fix$line[j - 1])) next
    
    # same line
    if (dat$trial[[trial]]$fix$line[j - 1] == dat$trial[[trial]]$fix$line[j]) {
      dat$trial[[trial]]$fix$sac.in[j] <- dat$trial[[trial]]$fix$letternum[j] - 
        dat$trial[[trial]]$fix$letternum[j - 1]
    }
    
    # go to line ahead
    if (dat$trial[[trial]]$fix$line[j - 1] < dat$trial[[trial]]$fix$line[j]) {
      dat$trial[[trial]]$fix$sac.in[j] <- 
        (dat$trial[[trial]]$fix$letternum[j] - min(dat$trial[[trial]]$meta$stimmat$letternum[dat$trial[[trial]]$meta$stimmat$line == dat$trial[[trial]]$fix$line[j]])) - 
        (dat$trial[[trial]]$fix$letternum[j - 1] - min(dat$trial[[trial]]$meta$stimmat$letternum[dat$trial[[trial]]$meta$stimmat$line == dat$trial[[trial]]$fix$line[j - 1]]))
    }
    
    # return to line visited before
    if (dat$trial[[trial]]$fix$line[j - 1] > dat$trial[[trial]]$fix$line[j]) {
      dat$trial[[trial]]$fix$sac.in[j] <- 
        (dat$trial[[trial]]$fix$letternum[j - 1] - min(dat$trial[[trial]]$meta$stimmat$letternum[dat$trial[[trial]]$meta$stimmat$line == dat$trial[[trial]]$fix$line[j - 1]])) - 
        (dat$trial[[trial]]$fix$letternum[j] - min(dat$trial[[trial]]$fix$letternum[dat$trial[[trial]]$fix$line == dat$trial[[trial]]$fix$line[j]]))
    }
    
  }
  
  # outgoing
  dat$trial[[trial]]$fix$sac.out <- NA
  
  for (j in 1:(nrow(dat$trial[[trial]]$fix) - 1)){
   
    if (is.na(dat$trial[[trial]]$fix$line[j]) | is.na(dat$trial[[trial]]$fix$line[j + 1])) next
    
    # same line
    if (dat$trial[[trial]]$fix$line[j + 1] == dat$trial[[trial]]$fix$line[j]) {
      dat$trial[[trial]]$fix$sac.out[j] <- dat$trial[[trial]]$fix$letternum[j + 1] - 
        dat$trial[[trial]]$fix$letternum[j] 
    }
    
    # go to line ahead
    if (dat$trial[[trial]]$fix$line[j + 1] > dat$trial[[trial]]$fix$line[j]) {
      dat$trial[[trial]]$fix$sac.out[j] <- 
        (dat$trial[[trial]]$fix$letternum[j + 1] - min(dat$trial[[trial]]$meta$stimmat$letternum[dat$trial[[trial]]$meta$stimmat$line == dat$trial[[trial]]$fix$line[j + 1]])) - 
        (dat$trial[[trial]]$fix$letternum[j] - min(dat$trial[[trial]]$meta$stimmat$letternum[dat$trial[[trial]]$meta$stimmat$line == dat$trial[[trial]]$fix$line[j]]))
    }
    
    # return to line visited before
    if (dat$trial[[trial]]$fix$line[j + 1] < dat$trial[[trial]]$fix$line[j]) {
      dat$trial[[trial]]$fix$sac.out[j] <- 
        (dat$trial[[trial]]$fix$letternum[j] - min(dat$trial[[trial]]$meta$stimmat$letternum[dat$trial[[trial]]$meta$stimmat$line == dat$trial[[trial]]$fix$line[j]])) - 
        (dat$trial[[trial]]$fix$letternum[j + 1] - min(dat$trial[[trial]]$meta$stimmat$letternum[dat$trial[[trial]]$meta$stimmat$line == dat$trial[[trial]]$fix$line[j + 1]]))
    }
  }
  
  return(dat)
  
}