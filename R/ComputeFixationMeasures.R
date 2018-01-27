
# NOTE: measures depend on IA, not word at present
# NOTE: seperate word/word? if yes, seperate measures or seperate slots?

ComputeFixationMeasures <- function(dat) {
  
  for (trial in 1:length(dat$trial)) {
    # trial <- 2
    
    dat <- ComputeSaccadeLength(dat, trial) 
    # NOTE: does only depend on letter -> maybe move to MatchStim()?
    
    dat <- ComputeLandingPosition(dat, trial)
    dat <- ComputeLaunchDistance(dat, trial)
    
    # TODO: combine?
    # TODO: for word/IA seperately?
    
    dat <- ComputeRefixation(dat, trial)
    dat <- ComputeRegression(dat, trial)
    dat <- ComputeFirstskip(dat, trial)
    # TODO: for word/IA seperately?
    
    dat <- ComputeRun(dat, trial)
    # TODO: for word/IA seperately
    
    # print(trial)
    
  }
  
 return(dat) 
 
}