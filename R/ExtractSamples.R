
ExtractSamples <- function(infile, env = parent.frame(n = 2)) {
  
  if (env$exp$setup$tracker$model == "eyelink") {
  
  options(warn = -1)
    
  # remove messages
  dat <- infile[-grep(paste(c("^\t", "^>", "^ ", "\\*", "DISPLAY", "INPUT",
                           "START", "PRESCALER", "PUPIL", "EVENTS", "SAMPLE",
                           "RETRACE", "!CAL", "VALIDATE", "RECCFG", "RECCFG", 
                           "ELCLCFG", "GAZE", "THRESHOLDS", "ELCL_PROC", 
                           "ELCL_PCR_PARAM", "!MODE", "DRIFTCORRECT", "!V", 
                           "MSG", "SSACC", "ESACC", "SFIX", "EFIX", 
                           "SBLINK", "EBLINK", "BUTTON"), collapse = "|"), 
                   infile, useBytes=TRUE)]
  # FIX: if stimulus is included in data
  dat <- dat[-grep("[[:alpha:]]", dat)]
  dat <- dat[nchar(dat) > 0]
  
  # select only elements with more than two elements
  dat <- dat[sapply(strsplit(dat, "\t"), length) > 3]
  # NOTE: assumes structure timestamp, x, y, pupil, ..., ...

  # extract variables
  time <- as.numeric(sapply(strsplit(dat, "\t"), "[[", 1))
  x <- as.numeric(gsub(" ", "", sapply(strsplit(dat, "\t"), "[[", 2)))
  y <- as.numeric(gsub(" ", "", sapply(strsplit(dat, "\t"), "[[", 3)))
  pupil <- as.numeric(gsub(" ", "", sapply(strsplit(dat, "\t"), "[[", 4)))
  out <- data.frame(time = time, x = x , y = y, pupil = pupil)
  
  return(out)
  
  options(warn = 0)
  
  } else if (env$exp$setup$tracker$model == "gazepoint") {
    
    dat <- infile[["data_collection/events/eyetracker/BinocularEyeSampleEvent"]]
    dat <- dat[]
    
    dat$gaze_x <- round(apply(dat[c("left_gaze_x", "right_gaze_x")], 1, mean, na.rm=TRUE))
    dat$gaze_y <- round(apply(dat[c("left_gaze_y", "right_gaze_y")], 1, mean, na.rm=TRUE))
    
    time <- round(dat$time*1000)
    x <- (env$exp$setup$display$resolutionX / 2) + round(dat$gaze_x)
    # x[x < 0] <- NA 
    y <- (env$exp$setup$display$resolutionY / 2) - round(dat$gaze_y)
    # y[y < 0] <- NA
    
    pupil <- round(apply(dat[c("left_pupil_measure1", "right_pupil_measure1")], 1, mean, na.rm=TRUE), 2)
    
    out <- data.frame(time = time, x = x , y = y, pupil = pupil)
    
    return(out)
    
  }
  
}
