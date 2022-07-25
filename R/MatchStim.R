
MatchStim <- function(dat, trial, env = parent.frame(n = 1)) {
  
  for (trial in 1:length(dat$item)) {
    
    # message(paste("... Trial ", trial, sep = ""))
    
    dat <- BuildStimulusFrame(dat, trial)
    
    if (env$debug == "line") {
      return (dat)
    }
    
    dat <- AssignStim(dat, trial)
    
  }
  
  # check for empty trials 
  for (i in length(dat$item):1) {
    if (length(dat$item[[i]]$fix) == 0) {
      dat$item[[i]] <- NULL
      env$header$exclusion <- env$header$exclusion + 1
    }
  }
  
 return(dat) 

}