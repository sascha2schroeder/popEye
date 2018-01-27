
MatchStim <- function(dat, trial) {
  
  for (trial in 1:length(dat$trial)) {
    dat <- AssignLetters(dat, trial)
    dat <- RetrieveWordBoundary(dat, trial)
    dat <- AssignWords(dat, trial)
  }
  
 return(dat) 

}