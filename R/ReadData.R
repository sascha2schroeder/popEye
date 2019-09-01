
ReadData <- function(filepath, subid, env = parent.frame(n = 1)) {
  
  # TODO: check whether encoding options also work on other OS
  # TODO: check whether edf file can be read in directly
  
  # create names for files
  if (env$exp$setup$tracker$software == "EB") {
    # edffile <- paste(filepath, "/", subid, "/", subid, ".edf", sep = "")
    ascfile <- paste(filepath, subid, "/", subid, ".asc", sep = "")
  } else if(env$exp$setup$tracker$software == "ET") {
    # edffile <- paste(filepath, "/", subid, ".EDF", sep = "")
    ascfile <- paste(filepath, "/", subid, ".asc", sep = "")
  }
  
  # setwd
  oldwd <- getwd()
  
  if (env$exp$setup$tracker$software == "EB") {
    newwd <- paste(filepath, subid, "/",sep = "")
    # setwd(newwd)
  } else if (env$exp$setup$tracker$software == "ET") {
    # setwd(paste(filepath, sep = ""))  
  }
  
  # # check whether EDF file exists
  # if (file.exists(edffile) == F){
  #   message("... No EDF file in directory")
  #   setwd(oldwd)
  #   return()
  # }
  
  # extract raw data
  # if (file.exists(ascfile) == F) {
  #   message(".. Read EDF file")
  #   ConvertEDF(edffile)  
  # }
  
  # read ASC
  infile   <- readLines(ascfile, encoding = "UTF-8")
  
  # extract data
  ExtractSetup(infile)
  ExtractHeader(infile)
  msg   <- ExtractMsg(infile)
  samp  <- ExtractSamples(infile)
  event <- ExtractEvents(infile)
  
  # combine
  raw <- list(msg = msg, samp = samp, event = event)
  
  # return wd
  setwd(oldwd)
  
  return(raw)
  
}
