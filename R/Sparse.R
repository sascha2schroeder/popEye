
Sparse <- function(dat) {
  
  for (trial in 1:length(dat$trial)) {
    dat$trial[[trial]]$msg <- NULL
    dat$trial[[trial]]$samp <- NULL
    dat$trial[[trial]]$event <- NULL
  }
  
  return(dat)
}
