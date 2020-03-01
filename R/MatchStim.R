
MatchStim <- function(dat, trial) {
  
  for (trial in 1:length(dat$trial)) {
    
    dat <- BuildStimulusFrame(dat, trial)
    # return(dat)
    dat <- AssignStim(dat, trial)
    
  }
  
 return(dat) 

}