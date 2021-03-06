
ExtractSamples <- function(dat){
  
  options(warn = -1)
  
  # remove messages
  dat <- dat[-grep(paste(c("^\t", "^>", "^ ", "\\*", "DISPLAY", "INPUT",
                           "START", "PRESCALER", "PUPIL", "EVENTS", "SAMPLE",
                           "RETRACE", "!CAL", "VALIDATE", "RECCFG", "RECCFG", 
                           "ELCLCFG", "GAZE", "THRESHOLDS", "ELCL_PROC", 
                           "ELCL_PCR_PARAM", "!MODE", "DRIFTCORRECT", "!V", 
                           "MSG", "SSACC", "ESACC", "SFIX", "EFIX", 
                           "SBLINK", "EBLINK", "BUTTON"), collapse = "|"), 
                   dat, useBytes=TRUE)]
  dat <- dat[nchar(dat) > 0]
  
  # select only elements with more than two elements
  dat <- dat[sapply(strsplit(dat, "\t"), length) > 3]
  # NOTE: assuems structure timestamp, x, y, pupil, ..., ...

    
  # extract variables
  time = as.numeric(sapply(strsplit(dat, "\t"), "[[", 1))
  x = as.numeric(gsub(" ", "", sapply(strsplit(dat, "\t"), "[[", 2)))
  y = as.numeric(gsub(" ", "", sapply(strsplit(dat, "\t"), "[[", 3)))
  pupil = as.numeric(gsub(" ", "", sapply(strsplit(dat, "\t"), "[[", 4)))
  out <- data.frame(time = time, x = x , y = y, pupil = pupil)
  
  return(out)
  
  options(warn = 1)
}
