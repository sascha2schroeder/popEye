
RetrieveIABoundary <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # parse out ia delimiter and target indicator
  tmp <- dat$trial[[trial]]$meta$stim

  # NOTE: only relevant if word indicator (does not really make sense)
  tmp <- gsub(env$exp$setup$indicator$word, " ", tmp)
  
  # determine target IA
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    dat$trial[[trial]]$meta$target <- grep(env$exp$setup$indicator$target, unlist(strsplit(tmp, env$exp$setup$indicator$ia)))
  }
  
  # parse out target indicator
  tmp <- gsub(env$exp$setup$indicator$target, "", tmp)
  
  # # parse out ia indicator
  # if (env$exp$setup$indicator$ia != " ") {
  #   tmp <- gsub(env$exp$setup$indicator$ia, "", tmp)
  # }
  
  # determine IA boundaries
  ia.length <- sapply(strsplit(tmp, env$exp$setup$indicator$ia), nchar)
  ia.boundary <- 0
  for (j in 2:length(ia.length)){
    ia.boundary <- c(ia.boundary, sum(ia.length[1:(j - 1)]))
  }
  ia.boundary[length(ia.boundary) + 1] <- sum(ia.length)
 
  # save in stim slot
  dat$trial[[trial]]$meta$ia.boundary <- ia.boundary
  
  return(dat)
  
}
