
RemoveData <- function(dat, env = parent.frame(n = 1)) {
 
  dat <- RemoveTrials(dat)
  dat <- RemoveSamples(dat)
  
  return(dat)
  
}