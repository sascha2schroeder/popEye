
ComputeLandingPosition <- function(dat, trial) {
  
  # initialize
  dat$trial[[trial]]$fix$word.cland <- NA
  dat$trial[[trial]]$fix$ia.cland <- NA
  
  for (j in 1:nrow(dat$trial[[trial]]$fix)){
    # j <- 2
    
    # centered landing position (see Vitu et al., 2001)
    
    # word
    dat$trial[[trial]]$fix$word.cland <- 
      dat$trial[[trial]]$fix$word.land - (nchar(dat$trial[[trial]]$fix$word) + 1) / 2
    
    # IA
    dat$trial[[trial]]$fix$ia.cland <- 
      dat$trial[[trial]]$fix$ia.land - (nchar(dat$trial[[trial]]$fix$ia) + 1) / 2
    
    # TODO: compute relative landing position
    
  }
  
  return(dat)
  
}  