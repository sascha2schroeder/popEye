
ReadData <- function(filepath, subid, env = parent.frame(n = 1)) {
  
  # TODO: check whether encoding options also work on other OS
  # TODO: check whether edf file can be read in directly
 
  # create names for files
  if (env$exp$setup$tracker$software == "EB") {
    # infile <- paste(filepath, "/", subid, "/", subid, ".edf", sep = "")
    infile <- paste(filepath, subid, "/", subid, ".asc", sep = "")
  } else if(env$exp$setup$tracker$software == "ET") {
    # infile <- paste(filepath, "/", subid, ".EDF", sep = "")
    infile <- paste(filepath, "/", subid, ".asc", sep = "")
  } else if(env$exp$setup$tracker$software == "psychopy") {
    infile <- paste(filepath, "/", subid, ".hdf5", sep = "")
  }
  
  # setwd
  oldwd <- getwd()
  
  if (env$exp$setup$tracker$software == "EB") {
    newwd <- paste(filepath, subid, "/",sep = "")
  } 
  
  # # check whether EDF file exists
  # if (file.exists(infile) == F){
  #   message("... No EDF file in directory")
  #   setwd(oldwd)
  #   return()
  # }
  
  # extract raw data
  # if (file.exists(infile) == F) {
  #   message(".. Read EDF file")
  #   ConvertEDF(infile)  
  # }
 
  # read infile
  if (env$exp$setup$tracker$software == "EB" | env$exp$setup$tracker$software == "ET" ) {
    
    # infile <- readLines(infile, encoding = "UTF-8")
    infile <- readLines(infile)
    
  } else if(env$exp$setup$tracker$software == "psychopy") {
    
    library(hdf5r)
    infile <- H5File$new(infile, mode="r")
    infile 
    
  }
  
  # extract data
  ExtractSetup(infile)
  ExtractHeader(infile)
  
  msg <- ExtractMsg(infile)
  samp <- ExtractSamples(infile)
  event <- ExtractEvents(infile)
  
  # combine
  raw <- list(msg = msg, samp = samp, event = event)
  
  # return wd
  setwd(oldwd)
  
  return(raw)
  
}
