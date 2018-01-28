
RetrieveIABoundary <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # set ia.delim to word.delim (default)
  if (env$exp$setup$stimulus$ia == "") {
    ia.delim <- env$exp$setup$stimulus$word
  } else {
    ia.delim <- env$exp$setup$stimulus$ia
  }
  
  # parse out ia delimiter and target indicator
  tmp <- dat$trial[[trial]]$meta$stim
  tmp <- gsub(env$exp$setup$stimulus$word, " ", tmp)
  
  # determine target IA
  if (env$exp$setup$type == "target") {
    dat$trial[[trial]]$meta$target <- grep(env$exp$setup$stimulus$target, unlist(strsplit(tmp, ia.delim)))
  }
  tmp <- gsub(env$exp$setup$stimulus$target, "", tmp)
  
  # determine IA boundaries
  ia.length <- sapply(strsplit(tmp, ia.delim), nchar)
  ia.boundary <- 0
  for (j in 2:length(ia.length)){
    ia.boundary <- c(ia.boundary, sum(ia.length[1:(j - 1)]) + (j - 1))
  }
  ia.boundary[length(ia.boundary) + 1] <- sum(ia.length) + length(ia.length)
  
  # save in stim slot
  dat$trial[[trial]]$meta$ia.boundary <- ia.boundary
  
  return(dat)
  
}
