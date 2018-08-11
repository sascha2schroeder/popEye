
ComputeLandingPosition <- function(dat, trial) {
  
  # trial = 1
  
  # first value
  dat$trial[[trial]]$fix$word.land <- NA
  dat$trial[[trial]]$fix$ia.land <- NA
  
  if (dat$trial[[trial]]$fix$start[1] > 1) {
    dat$trial[[trial]]$fix$word.land[1] <- NA
    dat$trial[[trial]]$fix$ia.land[1] <- NA
  } else {
    dat$trial[[trial]]$fix$word.land[1] <- dat$trial[[trial]]$fix$letter[1]
    dat$trial[[trial]]$fix$ia.land[1] <- dat$trial[[trial]]$fix$letter[1]
  }
  
  # subsequent values 
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    if(is.na(dat$trial[[trial]]$fix$letter[j]) == T) next
    dat$trial[[trial]]$fix$word.land[j] <- dat$trial[[trial]]$fix$letter[j] - 
      dat$trial[[trial]]$meta$word.boundary[dat$trial[[trial]]$fix$wordnum[j]]
    dat$trial[[trial]]$fix$ia.land[j] <- dat$trial[[trial]]$fix$letter[j] - 
      dat$trial[[trial]]$meta$ia.boundary[dat$trial[[trial]]$fix$ianum[j]]
  }
  
  return(dat)
  
}  