
Sparse <- function(dat) {
  
  for (trial in 1:length(dat$item)) {
    dat$item[[trial]]$msg <- NULL
    dat$item[[trial]]$samp <- NULL
    dat$item[[trial]]$event <- NULL
  }
  
  return(dat)
}
