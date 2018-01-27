
RetrieveWordBoundary <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # # parse out ia.delim
  # if (is.na(ia.delim) == F) {
  #   tmp <- dat$trial[[trial]]$stim$text
  #   tmp <- gsub(ia.delim, "", tmp)
  # } else {
  #   tmp <- dat$trial[[trial]]$stim$text
  # }
  
  tmp <- dat$trial[[trial]]$meta$text
  
  # determine word boundaries
  word.length <- sapply(strsplit(tmp, env$exp$setup$stimulus$word), nchar)
  word.boundary <- 0
  for (j in 2:length(word.length)){
    word.boundary <- c(word.boundary, sum(word.length[1:(j - 1)]) + (j - 1))
  }
  word.boundary[length(word.boundary) + 1] <- sum(word.length) + length(word.length)
  
  # save in stim slot
  dat$trial[[trial]]$meta$word.boundary <- word.boundary
  
  return(dat)
  
}
