
LineInteractive_2 <- function(fix, stimmat, env = parent.frame(n = 1)) {
  
  print(env$trial)
  
  run <- as.numeric(unlist(dimnames(table(fix$linerun))))
  ym <- tapply(stimmat$ym, stimmat$line, max)
  lines <- as.numeric(unlist(dimnames(table(stimmat$line))))
  
  fix$linetmp <- fix$line
  fix$line <- NA
  
  for (i in 1:length(run)) {
    # i <- 1
    
    # plot
    plot(stimmat$xs, stimmat$ym, xlim = c(0, max(stimmat$xe)), 
         ylim = c(max(stimmat$ye), min(stimmat$ys)), type = "n",
         xlab = "x (px)", ylab = "y (px)", main = paste("Trial", env$trial))
    
    for (j in 1:max(lines)){
      rect(min(stimmat$xs[stimmat$line == j]), max(stimmat$ye[stimmat$line == j]), 
           max(stimmat$xe[stimmat$line == j]), min(stimmat$ys[stimmat$line == j]))  
    }
    # text(ym[-fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]], 
    #      labels = unlist(dimnames(ym[-fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]])), col = "black")  
    
    text(ym, labels = unlist(dimnames(ym)), col = "black")  
    
    # rect(min(stimmat$xs[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]), 
    #      max(stimmat$ye[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]), 
    #      max(stimmat$xe[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]), 
    #      min(stimmat$ys[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]),
    #      border = "red", lwd = 2)  
    # text(ym[max(fix$linetmp[fix$linerun == i], na.rm = T)], 
    #      labels = max(fix$linetmp[fix$linerun == i], na.rm = T), col = "red", font = 2)
    
    points(fix$xn, fix$yn, type = "l", cex = 1, col = "black")
    text(fix$xn, fix$yn, labels = as.character(fix$line), col = "black", cex = 1)
    
    points(fix$xn[fix$linerun == run[i]], fix$yn[fix$linerun == run[i]],
           pch = 16, type = "b", cex = 1.5, col = "blue")
    
    key <- readline(prompt = "Confirm line:")
    
    if (key == "") {
      fix$line[is.na(fix$linerun) == F & fix$linerun == run[i]] <- fix$linetmp[is.na(fix$linerun) == F & fix$linerun == run[i]]  
    } else {
      fix$line[is.na(fix$linerun) == F & fix$linerun == run[i]] <- as.numeric(key)  
    }
    
  }
  
  fix$type[fix$line == 0] <- "out"
  fix$line[fix$line == 0] <- NA
  
  # final plot
  plot(stimmat$xs, stimmat$ym, xlim = c(0, max(stimmat$xe)), 
       ylim = c(max(stimmat$ye), min(stimmat$ys)), type = "n",
       xlab = "x (px)", ylab = "y (px)", main = paste("Trial", env$trial))
  
  for (j in 1:max(lines)){
    rect(min(stimmat$xs[stimmat$line == j]), max(stimmat$ye[stimmat$line == j]), 
         max(stimmat$xe[stimmat$line == j]), min(stimmat$ys[stimmat$line == j]))  
  }
  text(ym, labels = unlist(dimnames(ym)), col = "black")
  
  points(fix$xn, fix$yn, type = "l", cex = 1, col = "black")
  points(fix$xn, fix$yn, pch = 16, type = "p", cex = 1.25, col = fix$line)
  
  
  return(fix)
  
}

