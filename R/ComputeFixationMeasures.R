
# NOTE: measures depend on IA, not word at present
# NOTE: seperate word/word? if yes, seperate measures or seperate slots?

ComputeFixationMeasures <- function(dat) {
  
  for (trial in 1:length(dat$trial)) {
    # trial <- 2
    
    # TODO: ComputeLineChange
    
    dat <- OutlierAsBlinks(dat, trial) 
    # NOTE: not sure whether this function makes sense
    
    dat <- ComputeLineChange(dat, trial) 
    dat <- ComputeSaccadeLength(dat, trial) 
    # NOTE: does only depend on letter not IA or word -> maybe move to MatchStim()?
    
    dat <- ComputeLandingPosition(dat, trial)
    dat <- ComputeLaunchDistance(dat, trial)
    # # NOTE: combine in one function?
     
    dat <- ComputeRefixation(dat, trial)
    dat <- ComputeRegression(dat, trial)
    dat <- ComputeFirstskip(dat, trial)
    # NOTE: check for consistency with other skiping variables
    dat <- ComputeRun(dat, trial)
    
    # print(trial)
    
  }
  
 return(dat) 
 
}