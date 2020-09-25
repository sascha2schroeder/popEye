
ExtractFixations <- function(dat) {
  
  for (trial in 1:length(dat$item)) {
    
    dat <- RetrieveFixations(dat, trial)
    dat <- ComputeDur(dat, trial)
    
  }
  
  return(dat) 
  
}