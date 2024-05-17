
ProcessSaccades <- function(dat) {
  
  for (trial in 1:length(dat$item)) {
    dat <- RetrieveSaccades(dat, trial)
    dat <- ComputeSaccadeMeasures(dat, trial)
  }
  
  return(dat)
  
}