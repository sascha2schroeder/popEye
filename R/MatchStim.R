
MatchStim <- function(dat, trial, env = parent.frame(n = 1)) {
  
  for (trial in 1:length(dat$trial)) {
    
    message(paste("... Trial ", trial, sep = ""))
    
    dat <- BuildStimulusFrame(dat, trial)
    dat <- AssignStim(dat, trial)
    
  }
  
 return(dat) 

}