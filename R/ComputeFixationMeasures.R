
# NOTE: measures depend on IA, not word at present
# NOTE: seperate word/word? if yes, seperate measures or seperate slots?

ComputeFixationMeasures <- function(dat) {
  
  for (trial in 1:length(dat$trial)) {
    # trial <- 2
    
    dat <- ComputeSaccadeLength(dat, trial) 
    # NOTE: does only depend on letter not IA or word -> maybe move to MatchStim()?
    
    dat <- ComputeLandingPosition(dat, trial)
    dat <- ComputeLaunchDistance(dat, trial)
    # TODO: combine in one function?
    
    dat <- ComputeRefixation(dat, trial)
    dat <- ComputeRegression(dat, trial)
    dat <- ComputeFirstskip(dat, trial)
    dat <- ComputeRun(dat, trial)
    
    # print(trial)
    
  }
  
 return(dat) 
 
}