
ComputeFixationMeasures <- function(dat) {
  
  for (trial in 1:length(dat$trial)) {
    # trial <- 2
    
    # dat <- OutlierAsBlinks(dat, trial) 
    # NOTE: not sure whether this function makes sense
    
    dat <- ComputeLineChange(dat, trial) 
    dat <- ComputeSaccadeLength(dat, trial) 
    dat <- ComputeLaunchDistance(dat, trial)
    dat <- ComputeRefixation(dat, trial)
    dat <- ComputeRegression(dat, trial)
    dat <- ComputeFirstskip(dat, trial)
    dat <- ComputeRun(dat, trial)
    
    # print(trial)
    
  }
  
 return(dat) 
 
}