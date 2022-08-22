
ComputeLandingPosition <- function(dat, trial) {
  
  # initialize
  dat$item[[trial]]$fix$word.cland <- NA
  dat$item[[trial]]$fix$ia.cland <- NA
  
  for (j in 1:nrow(dat$item[[trial]]$fix)){
    # j <- 2
    
    # centered landing position (see Vitu et al., 2001)
    
    # word
    dat$item[[trial]]$fix$word.cland <- 
      dat$item[[trial]]$fix$word.land - (nchar(dat$item[[trial]]$fix$word) + 1) / 2
    
    # IA
    dat$item[[trial]]$fix$ia.cland <- 
      dat$item[[trial]]$fix$ia.land - (nchar(dat$item[[trial]]$fix$ia) + 1) / 2
    
    # TODO: compute relative landing position
    
  }
  
  return(dat)
  
}  