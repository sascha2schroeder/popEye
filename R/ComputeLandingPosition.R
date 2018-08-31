
ComputeLandingPosition <- function(dat, trial) {
  
  # trial = 1
  
  # NOTE: landing position still correct (space before = 0)?
  
  # first value
  dat$trial[[trial]]$fix$word.land <- NA
  dat$trial[[trial]]$fix$ia.land <- NA
  
  if (dat$trial[[trial]]$fix$start[1] > 1) {
    dat$trial[[trial]]$fix$word.land[1] <- NA
    dat$trial[[trial]]$fix$ia.land[1] <- NA
  } else {
    dat$trial[[trial]]$fix$word.land[1] <- dat$trial[[trial]]$fix$letternum[1]
    dat$trial[[trial]]$fix$ia.land[1] <- dat$trial[[trial]]$fix$letternum[1]
  }
  
  # subsequent values 
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    if(is.na(dat$trial[[trial]]$fix$letternum[j]) == T) next
    # NOTE: can this still happen?
    dat$trial[[trial]]$fix$word.land[j] <- dat$trial[[trial]]$fix$letternum[j] - 
      min(dat$trial[[trial]]$meta$stimmat$letno[dat$trial[[trial]]$meta$stimmat$word == dat$trial[[trial]]$fix$wordnum[j]])
    dat$trial[[trial]]$fix$ia.land[j] <- dat$trial[[trial]]$fix$letternum[j] - 
      min(dat$trial[[trial]]$meta$stimmat$letno[dat$trial[[trial]]$meta$stimmat$ia == dat$trial[[trial]]$fix$ianum[j]])
  }
  
  return(dat)
  
}  