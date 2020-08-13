
MatchStim <- function(dat, trial, env = parent.frame(n = 1)) {
  
  for (trial in 1:length(dat$trial)) {
      
    # message(paste("... Trial ", trial, sep = ""))
    
    dat <- BuildStimulusFrame(dat, trial)
    dat <- AssignStim(dat, trial)
    
  }
  
  # check for empty trials 
  for (i in length(dat$trial):1) {
    if (length(dat$trial[[i]]$fix) == 0) {
      dat$trial[[i]] <- NULL
    }
  }
  
 return(dat) 

}