
ExtractHeader <- function(infile, env = parent.frame(n = 2)){
  
  # date
  tmp <- infile[grep("DATE:", infile)]
  env$header$date <- paste(unlist(strsplit(tmp, " "), "[[")[c(4, 5, 7, 6)], collapse = " ")

    
  # calibration
  # ------------
  
  tmp <- infile[grep("MSG", infile)]
  tmp <- tmp[grep("VALIDATION", tmp)]
  
  if (sum(grepl("CALIBRATION", infile)) > 0) {
  
    tmp <- infile[grep("MSG", infile)]
    tmp <- tmp[grep("CALIBRATION", tmp)]
    tmp <- tmp[length(tmp)]
    # tmp <- tmp[grep("GOOD", tmp)]
    
    env$header$calibration$method <- sapply(strsplit(tmp, " "), "[[", 4)
    env$header$calibration$eye <- sapply(strsplit(tmp, " "), "[[", 5)
    
  }
  
  if (sum(grepl("VALIDATION", infile)) > 0) {
    
    tmp <- infile[grep("VALIDATION", infile)]
    
    tmp <- gsub("  ", " ", tmp)
    
    # check for aborted calibrations
    if (length(grep("ABORTED", tmp)) > 0) {
      tmp <- tmp[-grep("ABORTED", tmp)]  
    }
    
    # set up calibration matrix
    env$header$calibration <- data.frame(matrix(NA, length(tmp), 8))
    colnames(env$header$calibration) <- c("time", "eye", "method", 
                                          "avg", "max", "offset", 
                                          "x.px", "y.px")
    
    env$header$calibration$time <- 
      as.numeric(sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 1), "\t"), "[[", 2))
    env$header$calibration$method <- sapply(strsplit(tmp, " "), "[[", 4)
    env$header$calibration$eye <- sapply(strsplit(tmp, " "), "[[", 5)
    env$header$calibration$avg <- sapply(strsplit(tmp, " "), "[[", 9)
    env$header$calibration$max <- sapply(strsplit(tmp, " "), "[[", 11)
    env$header$calibration$offset <- sapply(strsplit(tmp, " "), "[[", 14)
    env$header$calibration$x.px <- as.numeric(sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 16), ","), "[[", 1))
    env$header$calibration$y.px <- as.numeric(sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 16), ","), "[[", 2))
    
  }
  
}