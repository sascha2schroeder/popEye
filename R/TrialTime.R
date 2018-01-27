
TrialTime <- function(tmp) {

  tmp$samp$time <- tmp$samp$time - tmp$msg$time[1] + 1
  tmp$event$time <- tmp$event$time - tmp$msg$time[1] + 1
  tmp$msg$time <- tmp$msg$time - tmp$msg$time[1] + 1
  
  return(tmp)
  
}