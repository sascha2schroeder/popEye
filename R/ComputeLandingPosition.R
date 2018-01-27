
ComputeLandingPosition <- function(dat, trial) {
  
  # trial = 1
  
  dat$trial[[trial]]$fix$land <- NA
  if (dat$trial[[trial]]$fix$start[1] > 1) {
    dat$trial[[trial]]$fix$land[1] <- NA
  } else {
    dat$trial[[trial]]$fix$land[1] <- dat$trial[[trial]]$fix$letter[1]
  }
  
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    if(is.na(dat$trial[[trial]]$fix$letter[j]) == T) next
    dat$trial[[trial]]$fix$land[j] <- dat$trial[[trial]]$fix$letter[j] - 
      dat$trial[[trial]]$meta$ia.boundary[dat$trial[[trial]]$fix$ia[j]]
  }
  
  return(dat)
  
}  