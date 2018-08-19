
ExtractAll <- function(ascfile = ascfile, subid = subid, env = parent.frame(n = 2)) {
  
  # read ASC
  infile   <- readLines(ascfile, encoding = "UTF-8")

  # compute frames
  msg   <- ExtractMsg(infile)
  samp  <- ExtractSamples(infile)
  event <- ExtractEvents(infile)
  
  # combine
  raw <- list(msg = msg, samp = samp, event = event)
  
  return(raw)
  
}
