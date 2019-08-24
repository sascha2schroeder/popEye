
ExtractSetup <- function(infile, env = parent.frame(n = 2)){
  
  # display resolution
  tmp <- infile[grep("GAZE_COORDS", infile)][1]
  env$exp$setup$display$resolutionX <-
    as.numeric(sapply(strsplit(tmp, " "), "[[", 5)[1]) + 1
  env$exp$setup$display$resolutionY <-
    as.numeric(sapply(strsplit(tmp, " "), "[[", 6)[1]) + 1
  
  # monitor refresh rate
  if (sum(grepl("RETRACE_INTERVAL", infile)) > 0) {
    tmp <- infile[grep("RETRACE_INTERVAL", infile)]
    env$exp$setup$display$refresh <- round(as.numeric(sapply(strsplit(tmp, " "), "[[", 4)), 2)
  } else if (sum(grepl("FRAMERATE", infile)) > 0) {
    tmp <- infile[grep("FRAMERATE", infile)]
    env$exp$setup$display$refresh <- round(1000 / as.numeric(sapply(strsplit(tmp, " "), "[[", 3)), 2)
  }
  
  # tracker sample rate
  tmp <- infile[grep("!MODE RECORD", infile)][1]
  env$exp$setup$tracker$samp <- as.numeric(sapply(strsplit(tmp, " "), "[[", 5))

}
