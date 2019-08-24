
ExtractHeader <- function(infile, env = parent.frame(n = 2)){
  
  # date
  tmp <- infile[grep("DATE:", infile)]
  env$header$date <- paste(unlist(strsplit(tmp, " "), "[[")[c(4, 5, 7, 6)], collapse = " ")

    
  # calibration
  # ------------
  
  tmp <- infile[grep("VALIDATION", infile)]
  
  # check for aborted calibrations
  if (length(grep("ABORTED", tmp)) > 0) {
    tmp <- tmp[-grep("ABORTED", tmp)]  
  }
  
  # set up calibration matrix
  env$header$calibration <- data.frame(matrix(NA, length(tmp), 7))
  colnames(env$header$calibration) <- c("time", "method", "avg", "max", "offset", 
                                        "x.px", "y.px")
  
  env$header$calibration$time <- 
    as.numeric(sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 1), "\t"), "[[", 2))
  env$header$calibration$method <- sapply(strsplit(tmp, " "), "[[", 4)
  env$header$calibration$avg <- sapply(strsplit(tmp, " "), "[[", 9)
  env$header$calibration$max <- sapply(strsplit(tmp, " "), "[[", 11)
  env$header$calibration$offset <- sapply(strsplit(tmp, " "), "[[", 15)
  env$header$calibration$x.px <- as.numeric(sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 17), ","), "[[", 1))
  env$header$calibration$y.px <- as.numeric(sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 17), ","), "[[", 2))
  
}