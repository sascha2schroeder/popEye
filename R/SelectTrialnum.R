
SelectTrialnum <- function(sub, trial) {
  
  # tr <- sub$trial[[which(lapply(lapply(sub$trial, "[[", "meta"), "[[", "trialnum") == trial)]]
  tr <- sub$trial[[trial]]
  
  return(tr)
  
}
