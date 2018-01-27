
MatchIA <- function(dat) {
  
  for (trial in 1:length(dat$trial)) {
    dat <- RetrieveIABoundary(dat, trial)
    dat <- AssignIA(dat, trial)
  }
  
  return(dat)
  
}
