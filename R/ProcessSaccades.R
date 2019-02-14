
ProcessSaccades <- function(dat) {
  
  # message(".. compute saccades")
  
  for (trial in 1:length(dat$trial)) {
    dat <- RetrieveSaccades(dat, trial)
    dat <- ComputeSaccadeMeasures(dat, trial)
  }
  
  # message(".. done")
  
  return(dat)
  
}