
RetrieveLetterBoundary <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # parse out ia delimiter and target indicator
  tmp <- dat$trial[[trial]]$meta$stim
  tmp <- gsub(env$exp$setup$stimulus$target, "", tmp)
  tmp <- gsub(env$exp$setup$stimulus$ia, "", tmp)
 
  # determine word boundaries
  letters <- unlist(strsplit(tmp, ""))
  letter.boundary <- env$exp$setup$display$marginX
  letpix <- env$exp$setup$font$letpix
  for (i in 1:length(letters)){
    letter.boundary <- c(letter.boundary, letter.boundary[length(letter.boundary)] 
                         + letpix$pixel[letpix$letter == letters[i]] + 1)
  }
  
  # save in stim slot
  dat$trial[[trial]]$meta$letter.boundary <- letter.boundary
  
  return(dat)
  
}