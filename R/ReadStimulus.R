
ReadStimulus <- function(dat, env = parent.frame(n = 1)) {

  stimfile <- env$exp$setup$stimulus$file
  dat <- ExtractStimulus(dat, stimfile)
  
  return(dat)
  
}