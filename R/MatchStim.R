
MatchStim <- function(dat, trial) {
  
  for (trial in 1:length(dat$trial)) {
  # for (trial in 81:81) {
    dat <- BuildStimulusFrame(dat, trial)
    dat <- AssignStim(dat, trial)
  }
  
 return(dat) 

}