
LineInteractive_2 <- function(fix, stimmat, env = parent.frame(n = 1)) {
  
  print(env$trial)
  
  run <- as.numeric(unlist(dimnames(table(fix$linerun))))
  ym <- tapply(stimmat$ym, stimmat$line, max)
  lines <- as.numeric(unlist(dimnames(table(stimmat$line))))
  
  # cols <- palette.colors(max(lines))
  
  fix$linetmp <- fix$line
  fix$line <- NA
  
  for (i in 1:length(run)) {
    # i <- 1
    
    # basic plot
    plot(stimmat$xs, stimmat$ym, xlim = c(0, max(stimmat$xe)), 
         ylim = c(max(stimmat$ye), min(stimmat$ys)), type = "n",
         xlab = "x (px)", ylab = "y (px)", main = paste("Trial", env$trial))
    
    for (j in 1:max(lines)){
      
      # # black
      # rect(min(stimmat$xs[stimmat$line == j]), max(stimmat$ye[stimmat$line == j]), 
      #      max(stimmat$xe[stimmat$line == j]), min(stimmat$ys[stimmat$line == j]),
      #      border = "black")  
      
      # colored
      rect(min(stimmat$xs[stimmat$line == j]), max(stimmat$ye[stimmat$line == j]), 
           max(stimmat$xe[stimmat$line == j]), min(stimmat$ys[stimmat$line == j]),
           border = lines[j])  
      
    }
    
    # labels
    # text(ym, labels = unlist(dimnames(ym)), col = "black") # black
    text(ym, labels = unlist(dimnames(ym)), col = lines) # colored
    
    # show preselected line
    # rect(min(stimmat$xs[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]),
    #      max(stimmat$ye[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]),
    #      max(stimmat$xe[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]),
    #      min(stimmat$ys[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]),
    #      border = fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i], lwd = 2)
    
    rect(min(stimmat$xs[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]),
         max(stimmat$ye[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]),
         max(stimmat$xe[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]),
         min(stimmat$ys[stimmat$line == fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i]]),
         border = fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i], lwd = 2,
         col = MakeTransparent(palette()[fix$linetmp[is.na(fix$linerun) == F & fix$linerun == i] %% max(lines)], alpha = .1))
    
    # text(ym[max(fix$linetmp[fix$linerun == i], na.rm = T)],
    #      labels = max(fix$linetmp[fix$linerun == i], na.rm = T), col = "red", font = 2)
    
    # show previous runs
    points(fix$xn, fix$yn, type = "l", cex = 1, col = "black")
    text(fix$xn, fix$yn, labels = as.character(fix$line), col = fix$line, cex = 1)
    
    # # show current run as points
    # points(fix$xn[fix$linerun == run[i]], fix$yn[fix$linerun == run[i]],
    #        pch = 16, type = "b", cex = 1.5, col = "blue")
    
    # show current run as number
    if (fix$linetmp[fix$linerun == run[i]] != 0) {
      coltmp <- fix$linetmp[fix$linerun == run[i]]
    } else {
      coltmp <- "grey"
    }
    text(fix$xn[fix$linerun == run[i]], fix$yn[fix$linerun == run[i]],
         labels = fix$linetmp[fix$linerun == run[i]], col = coltmp,
         font = 2, cex = 1)
    # symbols(fix$xn[fix$linerun == run[i]], fix$yn[fix$linerun == run[i]],
    #      circles = rep(1, length(fix$xn[fix$linerun == run[i]])), add = T)
    symbols(fix$xn[fix$linerun == run[i]], fix$yn[fix$linerun == run[i]],
            circles = fix$dur[fix$linerun == run[i]]/20, inches = F, fg = coltmp,
            add = T)
    
    # line input
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
         max(stimmat$xe[stimmat$line == j]), min(stimmat$ys[stimmat$line == j]),
         border = lines[j], lwd = 2,
         col = MakeTransparent(palette()[lines[j]], alpha = .1))  
  }
  text(ym, labels = unlist(dimnames(ym)), col = lines)
  
  ytmp <- jitter(ym[fix$line], .5)
  points(fix$xn, ytmp, type = "l", cex = 1, col = "black")
  points(fix$xn, ytmp, pch = 16, type = "p", cex = 1.25, col = fix$line)
  
  Sys.sleep(3)
  
  return(fix)
  
}

