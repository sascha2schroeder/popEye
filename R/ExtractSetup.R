
ExtractSetup <- function(infile, env = parent.frame(n = 2)){
  
  if (env$exp$setup$tracker$model == "eyelink") {
  
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
    
  } else if (env$exp$setup$tracker$model == "gazepoint") {
        
    msg = infile[["data_collection/events/experiment/MessageEvent"]]
    msg = msg[]
    
    # display resolution
    tmp <- msg$text[grep("DISPLAY ", msg$text)]
    env$exp$setup$display$resolutionX <- as.numeric(gsub("\\[", "", sapply(strsplit(tmp, " "), "[[", 2)))
    env$exp$setup$display$resolutionY <- as.numeric(gsub("\\]", "", sapply(strsplit(tmp, " "), "[[", 3)))
    
    # monitor refresh rate
    tmp <- msg$text[grep("REFRESH_RATE", msg$text)]
    env$exp$setup$display$refresh <- round(as.numeric(sapply(strsplit(tmp, " "), "[[", 2))*1000, 2)
    
    # tracker sample rate
    tmp <- msg$text[grep("SAMPLING_RATE", msg$text)]
    env$exp$setup$tracker$samp <- as.numeric(sapply(strsplit(tmp, " "), "[[", 2))
    
  }
  
}
