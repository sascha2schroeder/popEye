
ExtractHeader <- function(infile, env = parent.frame(n = 2)){
  
  
  if (env$exp$setup$tracker$model == "eyelink") {
    
    # date
    tmp <- infile[grep("DATE:", infile)]
    env$header$date <- paste(unlist(strsplit(tmp, " "), "[[")[c(4, 5, 7, 6)], collapse = " ")
    
    
    # calibration
    # ------------
    
    if (sum(grepl("CALIBRATION", infile)) > 0) {
      
      tmp <- infile[grep("MSG", infile)]
      tmp <- tmp[grep("CALIBRATION", tmp)]
      tmp <- tmp[length(tmp)]
      
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
    
  } else if (env$exp$setup$tracker$model == "gazepoint") {
    
    # extract msg
    msg = infile[["data_collection/events/experiment/MessageEvent"]]
    msg = msg[]
    
    # date
    tmp <- msg$text[grep("DATE", msg$text)]
    env$header$date <- sapply(strsplit(tmp, " "), "[[", 2)
    
    # set up calibration matrix
    tmp <- msg[grep("CALIBRATION VALID_POINTS", msg$text), ]
    env$header$calibration <- data.frame(matrix(NA, nrow(tmp), 4))
    colnames(env$header$calibration) <- c("time", "method", "points", "error")
    
    tmp <- msg[grep("CALIBRATION VALID_POINTS", msg$text), ]
    env$header$calibration$time <- tmp$time
    
    tmp <- msg[grep("CALIBRATION TYPE", msg$text), ]
    tmp_text <- sapply(strsplit(tmp$text, " "), "[[", 3)
    tmp_time <- round(tmp$time, 1)
    env$header$calibration$method <- tmp_text[duplicated(tmp_time) == F]
    
    tmp <- msg[grep("CALIBRATION VALID_POINTS", msg$text), ]
    env$header$calibration$points <- sapply(strsplit(tmp$text, " "), "[[", 3)
    env$header$calibration$points[env$header$calibration$points == "unknown"] <- NA
    
    tmp <- msg[grep("CALIBRATION ERROR", msg$text), ]
    env$header$calibration$error <- sapply(strsplit(tmp$text, " "), "[[", 3)
    env$header$calibration$error[env$header$calibration$error == "unknown"] <- 0
    
  }
  
}