
ComputeSaccadeLength <- function(dat, trial) {
  
  # trial = 1
  
  # NOTE: saccades from outlying fixations do not receive saccade lengths
  # NOTE: y distance is ignored in multi-line saccades
    
  # incoming
  dat$item[[trial]]$fix$sac.in <- NA
  
  for (j in 2:nrow(dat$item[[trial]]$fix)) {
    
    if (is.na(dat$item[[trial]]$fix$line[j]) | is.na(dat$item[[trial]]$fix$line[j - 1])) next
    
    # same line
    if (dat$item[[trial]]$fix$line[j - 1] == dat$item[[trial]]$fix$line[j]) {
      dat$item[[trial]]$fix$sac.in[j] <- dat$item[[trial]]$fix$letternum[j] - 
        dat$item[[trial]]$fix$letternum[j - 1]
    }
    
    # go to line ahead
    if (dat$item[[trial]]$fix$line[j - 1] < dat$item[[trial]]$fix$line[j]) {
      dat$item[[trial]]$fix$sac.in[j] <- 
        (dat$item[[trial]]$fix$letternum[j] - min(dat$item[[trial]]$meta$stimmat$letternum[dat$item[[trial]]$meta$stimmat$line == dat$item[[trial]]$fix$line[j]])) - 
        (dat$item[[trial]]$fix$letternum[j - 1] - min(dat$item[[trial]]$meta$stimmat$letternum[dat$item[[trial]]$meta$stimmat$line == dat$item[[trial]]$fix$line[j - 1]]))
    }
    
    # return to line visited before
    if (dat$item[[trial]]$fix$line[j - 1] > dat$item[[trial]]$fix$line[j]) {
      dat$item[[trial]]$fix$sac.in[j] <- 
        (dat$item[[trial]]$fix$letternum[j - 1] - min(dat$item[[trial]]$meta$stimmat$letternum[dat$item[[trial]]$meta$stimmat$line == dat$item[[trial]]$fix$line[j - 1]])) - 
        (dat$item[[trial]]$fix$letternum[j] - min(dat$item[[trial]]$fix$letternum[dat$item[[trial]]$fix$line == dat$item[[trial]]$fix$line[j]]))
    }
    
  }
  
  # outgoing
  dat$item[[trial]]$fix$sac.out <- NA
  
  for (j in 1:(nrow(dat$item[[trial]]$fix) - 1)){
   
    if (is.na(dat$item[[trial]]$fix$line[j]) | is.na(dat$item[[trial]]$fix$line[j + 1])) next
    
    # same line
    if (dat$item[[trial]]$fix$line[j + 1] == dat$item[[trial]]$fix$line[j]) {
      dat$item[[trial]]$fix$sac.out[j] <- dat$item[[trial]]$fix$letternum[j + 1] - 
        dat$item[[trial]]$fix$letternum[j] 
    }
    
    # go to line ahead
    if (dat$item[[trial]]$fix$line[j + 1] > dat$item[[trial]]$fix$line[j]) {
      dat$item[[trial]]$fix$sac.out[j] <- 
        (dat$item[[trial]]$fix$letternum[j + 1] - min(dat$item[[trial]]$meta$stimmat$letternum[dat$item[[trial]]$meta$stimmat$line == dat$item[[trial]]$fix$line[j + 1]])) - 
        (dat$item[[trial]]$fix$letternum[j] - min(dat$item[[trial]]$meta$stimmat$letternum[dat$item[[trial]]$meta$stimmat$line == dat$item[[trial]]$fix$line[j]]))
    }
    
    # return to line visited before
    if (dat$item[[trial]]$fix$line[j + 1] < dat$item[[trial]]$fix$line[j]) {
      dat$item[[trial]]$fix$sac.out[j] <- 
        (dat$item[[trial]]$fix$letternum[j] - min(dat$item[[trial]]$meta$stimmat$letternum[dat$item[[trial]]$meta$stimmat$line == dat$item[[trial]]$fix$line[j]])) - 
        (dat$item[[trial]]$fix$letternum[j + 1] - min(dat$item[[trial]]$meta$stimmat$letternum[dat$item[[trial]]$meta$stimmat$line == dat$item[[trial]]$fix$line[j + 1]]))
    }
    
  }
  
  return(dat)
  
}