
ReadData <- function(filepath, subid, env = parent.frame(n = 1)) {
  
  # TODO: integrate workflow for different setups (EyeTrack, ExperimentBuilder)
  # TODO: create subdirectories ?
  # TODO: check whether encoding options also work on other OS
  # TODO: add argument for EyeTrack workflow
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
    setwd(paste(filepath, sep = ""))  
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
  #   message(".. Extract data")
  #   raw <- ExtractAll(ascfile, subid)
  # } else {
    message(".. Extract data")
    raw <- ExtractAll(ascfile, subid)
  # } 
  
  # return wd
  setwd(oldwd)
  
  return(raw)
  
}
