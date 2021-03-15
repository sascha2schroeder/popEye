
SelectLine <- function(fix, stimmat) {
  
  run <- as.numeric(unlist(dimnames(table(fix$linerun))))
  lines <- as.numeric(unlist(dimnames(table(stimmat$line))))
  
  fix$line <- NA
  for (i in 1:length(run)) {
    # i <- 5
    
    fixtmp <- fix[is.na(fix$linerun) == F & fix$linerun == run[i], ]
    
    out <- NULL
    for (j in 1:length(lines)) {
      # j <- 12
      
      tmp <- matrix(NA, 1, 3)
      
      tmp[1, 1] <- nrow(fixtmp)
      tmp[1, 2] <- j
      
      hit <- vector(mode = "numeric", length = nrow(fixtmp))
      
      for (k in 1:nrow(fixtmp)) {
        # k <- 1
        
        if (fixtmp$xn[k] > min(stimmat$xs[stimmat$line == j]) &
            fixtmp$xn[k] < max(stimmat$xe[stimmat$line == j]) &
            fixtmp$yn[k] > min(stimmat$ys[stimmat$line == j]) &
            fixtmp$yn[k] <= max(stimmat$ye[stimmat$line == j])) {
          hit[k] <- 1
        }
        
        
      }
      
      tmp[1, 3] <- sum(hit)
      
      out <- rbind(out, tmp)
      
    }
    
    # select line
    if (sum(out[, 3]) == 0) {
      fix$line[fix$linerun == i] <- 0
    } else {
      fix$line[fix$linerun == i] <- min(which.max(out[, 3]))
    }
    
  } 
  
  
  # assign unassigned line to nearest line
  fixtmp <- fix[fix$line == 0, ]
  runs <- unlist(dimnames(table(fixtmp$linerun)))
  
  mrun <- tapply(fixtmp$yn[fixtmp$type == "in"], fixtmp$linerun[fixtmp$type == "in"], mean)
  mline <- tapply(stimmat$ys, stimmat$line, mean) + (tapply(stimmat$ye, stimmat$line, mean) - tapply(stimmat$ys, stimmat$line, mean)) / 2
  
  for (i in 1:length(mrun)) {
    # i <- 9

    out <- NULL
    for (j in 1:length(mline)) {
      # j <- 1

      out[j] <- (mrun[i]  - mline[j])^2

    }

    fix$line[fix$linerun == runs[i]] <- which.min(out)

  }
  
  return(fix)
  
}
