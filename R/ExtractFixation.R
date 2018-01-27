
ExtractFixation <- function(dat) {
  
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    dat <- RetrieveFixations(dat, trial) # creates fix object
    dat <- ComputeDur(dat, trial)
  }
  
 return(dat) 
  
}