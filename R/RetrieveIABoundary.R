
RetrieveIABoundary <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # set ia.delim to word.delim (default)
  if (is.na(env$exp$setup$stimulus$ia) == T) {
    env$exp$setup$stimulus$ia = env$exp$setup$stimulus$word
  }
  
  # determine IA boundaries
  ia.length <- sapply(strsplit(dat$trial[[trial]]$meta$text, env$exp$setup$stimulus$ia), nchar)
  ia.boundary <- 0
  for (j in 2:length(ia.length)){
    ia.boundary <- c(ia.boundary, sum(ia.length[1:(j - 1)]) + (j - 1))
  }
  ia.boundary[length(ia.boundary) + 1] <- sum(ia.length) + length(ia.length)
  
  # save in stim slot
  dat$trial[[trial]]$meta$ia.boundary <- ia.boundary
  
  return(dat)
  
}
