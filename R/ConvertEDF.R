
ConvertEDF <- function(edffile) {
  
  # NOTE: only works with Linux at present
  # NOTE: this is maybe better done with a bash script
  # NOTE: intended to be used with single files, but not directories at present
  # NOTE: assumes that you are already in working directory or that it is 
  #       part of the file name
  # TODO: go to folder ?
  # TODO: create subdirectories ?
  # TODO: check whether files exists ?
  
  system(paste("edf2asc", edffile, sep = " "), wait = T)
  
}
