
LineInteractive <- function(fix, stimmat, env = parent.frame(n = 1)) {
  
  print(env$trial)
  
  fixtmp <- fix[fix$type == "in", ]
  
  run <- as.numeric(unlist(dimnames(table(fixtmp$linerun))))
  ym <- tapply(stimmat$ym, stimmat$line, max)
  lines <- as.numeric(unlist(dimnames(table(stimmat$line))))
  
  fixtmp$linetmp <- fixtmp$line
  fixtmp$line <- NA
  fix$line <- NULL
  
  
  i <- 1
  while (i <=length(run)) {
    
    # print(paste("Run", i, "of", length(run)))
    
    # basic plot
    plot(stimmat$xs, stimmat$ym, xlim = c(0, max(stimmat$xe)), 
         ylim = c(max(stimmat$ye), min(stimmat$ys)), type = "n",
         xlab = "x (px)", ylab = "y (px)", main = paste("Trial", env$trial))
    
    for (j in 1:max(lines)){
      
      rect(min(stimmat$xs[stimmat$line == j]), max(stimmat$ye[stimmat$line == j]), 
           max(stimmat$xe[stimmat$line == j]), min(stimmat$ys[stimmat$line == j]),
           border = lines[j])  
      
    }
    
    # labels
    text(ym, labels = unlist(dimnames(ym)), col = lines) # colored
    
    rect(min(stimmat$xs[stimmat$line == fixtmp$linetmp[is.na(fixtmp$linerun) == F & fixtmp$linerun == i]]),
         max(stimmat$ye[stimmat$line == fixtmp$linetmp[is.na(fixtmp$linerun) == F & fixtmp$linerun == i]]),
         max(stimmat$xe[stimmat$line == fixtmp$linetmp[is.na(fixtmp$linerun) == F & fixtmp$linerun == i]]),
         min(stimmat$ys[stimmat$line == fixtmp$linetmp[is.na(fixtmp$linerun) == F & fixtmp$linerun == i]]),
         border = fixtmp$linetmp[is.na(fixtmp$linerun) == F & fixtmp$linerun == i], lwd = 2,
         col = MakeTransparent(palette()[fixtmp$linetmp[is.na(fixtmp$linerun) == F & fixtmp$linerun == i] %% 8], alpha = .1))
    
    # add words
    words <- as.numeric(unlist(dimnames(table(stimmat$wordnum))))
    for (j in 1:max(words)) {
      rect(min(stimmat$xs[stimmat$wordnum == words[j]]),
           min(stimmat$ys[stimmat$wordnum == words[j]]),
           max(stimmat$xe[stimmat$wordnum == words[j]]),
           max(stimmat$ye[stimmat$wordnum == words[j]]),
           angle = NA, lwd = 1, border = MakeTransparent(palette()[stimmat$line[stimmat$wordnum == words[j]] %% 8], alpha = .1))
    }
    
    # show previous runs
    # points(fixtmp$xn, fixtmp$yn, type = "l", cex = 1, col = "black")
    # text(fixtmp$xn, fixtmp$yn, labels = as.character(fixtmp$line), col = fixtmp$line, cex = 1)
    points(fixtmp$xn, fixtmp$yn, type = "l", cex = 1, col = "black")
    points(fixtmp$xn, fixtmp$yn, type = "p", cex = .75, col = fixtmp$line, pch=16)
    
    # show current run as number
    if (fixtmp$linetmp[fixtmp$linerun == run[i]] != 0) {
      coltmp <- fixtmp$linetmp[fixtmp$linerun == run[i]]
    } else {
      coltmp <- "grey"
    }
    text(fixtmp$xn[fixtmp$linerun == run[i]], fixtmp$yn[fixtmp$linerun == run[i]],
         labels = fixtmp$linetmp[fixtmp$linerun == run[i]], col = coltmp,
         font = 2, cex = 1.5)
    # text(fixtmp$xn[fixtmp$linerun == run[i]], fixtmp$yn[fixtmp$linerun == run[i]],
    #      labels = fixtmp$linetmp[fixtmp$linerun == run[i]], col = "red",
    #      font = 2, cex = 1.5)
    # symbols(fixtmp$xn[fixtmp$linerun == run[i]], fixtmp$yn[fixtmp$linerun == run[i]],
    #         circles = fixtmp$dur[fixtmp$linerun == run[i]]/10, inches = F, fg = coltmp,
    #         add = T)
    # symbols(fixtmp$xn[fixtmp$linerun == run[i]], fixtmp$yn[fixtmp$linerun == run[i]],
    #         circles = fixtmp$dur[fixtmp$linerun == run[i]]/10, inches = F, fg = "red",
    #         add = T)
    
    # line input
    key <- readline(prompt = "Confirm line:")
    
    if (key == "") {
      fixtmp$line[is.na(fixtmp$linerun) == F & fixtmp$linerun == run[i]] <- fixtmp$linetmp[is.na(fixtmp$linerun) == F & fixtmp$linerun == run[i]]  
      i <- i + 1
    } else if (key == "b") {
      i <- i - 1
      next
    } else {
      fixtmp$line[is.na(fixtmp$linerun) == F & fixtmp$linerun == run[i]] <- as.numeric(key)  
      i <- i + 1
    }
    
  }
  
  fixtmp$type[fixtmp$line == 0] <- "out"
  fixtmp$line[fixtmp$line == 0] <- NA
  
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
  
  ytmp <- jitter(ym[fixtmp$line], .5)
  points(fixtmp$xn, ytmp, type = "l", cex = 1, col = "black")
  points(fixtmp$xn, ytmp, pch = 16, type = "p", cex = 1.25, col = fixtmp$line)
  
  Sys.sleep(3)
  
  fix <- merge(fix, fixtmp[c("num", "line")], all=T, by="num")
  
  # set type out for fixations without line assignment
  # NOTE: does this make sense?
  fix$type[is.na(fix$line) == T] <- "out"
  
  return(fix)
  
}

