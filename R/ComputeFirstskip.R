
ComputeFirstskip <- function(dat, trial) {
  
  # trial <- 2
  
  dat$trial[[trial]]$fix$firstskip <- 0
  mem <- 0
  
  for (j in 1:nrow(dat$trial[[trial]]$fix)){
    # j <- 1
    if (dat$trial[[trial]]$fix$ia[j] < max(mem) & 
        is.element(dat$trial[[trial]]$fix$ia[j], mem) == F) {
     dat$trial[[trial]]$fix$firstskip[j] <- 1 
    }
    mem <- c(mem, dat$trial[[trial]]$fix$ia[j])
    # print(j)
  }  
  
  return(dat)
  
}
