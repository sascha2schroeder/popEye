
RetrieveWordBoundary <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # parse out ia delimiter and target indicator
  tmp <- dat$trial[[trial]]$meta$stim
  tmp <- gsub(env$exp$setup$indicator$target, "", tmp)
  if (env$exp$setup$indicator$ia != " ") {
    tmp <- gsub(env$exp$setup$indicator$ia, "", tmp)
  }
 
  # determine word boundaries
  word.length <- sapply(strsplit(tmp, env$exp$setup$indicator$word), nchar)
  word.boundary <- 0
  for (j in 2:length(word.length)){
    word.boundary <- c(word.boundary, sum(word.length[1:(j - 1)]) + (j - 1) - 1)
  }
  word.boundary[length(word.boundary) + 1] <- sum(word.length) + length(word.length) - 1
  
  # NOTE: how to deal with interpunctation?
  
  # save in stim slot
  dat$trial[[trial]]$meta$word.boundary <- word.boundary
  
  return(dat)
  
}
