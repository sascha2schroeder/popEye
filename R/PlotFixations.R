
PlotFixations <- function(exp, subject, trial, start = 1, stop = NULL, 
                          pdf = NULL, interactive = F) {
  
  fix <- SelectSubjectTrial(exp, subject, trial)$fix
  
  # set colors
  if (max(fix$line, na.rm = T) > 1) {
    # palette(topo.colors(max(fix$line)))
    palette(rainbow(max(fix$line, na.rm = T)))
  } else {
    palette(topo.colors(2))
  }
  
  if (missing(stop)) {
    end <- 1
  } else {
    end <- stop
  }
  
  while(end <= max(fix$fixid)) {
    
    # start pdf
    if (missing(pdf) == T) {
      par(mfrow = c(1, 1), cex = 1.25, oma = c(0, 0, 0, 0))
    } else {
      pdf(pdf, width = 16, height = 8.5)
      par(mfrow = c(1, 1), cex = .9, oma = c(0, 0, 2, 0))
    }
    
    PlotStimulus(exp, subject, trial)
    
    points(fix$xs[start:end],
           fix$ys[start:end],
           col = "black",
           pch = 16, type = "l")
    
    if (fix$type[start:end] == "out") {
      points(fix$xs[start:end],
             fix$ys[start:end],
             col = "black",
             pch = 16, cex = 1, type = "p")
    }
    
    points(fix$xs[start:end],
           fix$ys[start:end],
           col = fix$line[start:end],
           pch = 16, cex = 1, type = "p")
    
    points(fix$xs[end],
           fix$ys[end],
           col = fix$line[end],
           pch = 16, cex = 2, type = "p")
    
    # add fixation number
    text(fix$xn[end], (fix$yn[end] - 20), fix$fixid[end], col = "black", cex = 1.25)  
    
    
    # turn off device
    if (missing(pdf) == F) {
      dev.off()
    } else {
      par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
    }
    
    if (interactive == T) {
      line <- readline()
      if (line == "") end <- end + 1
      if (line == "q") break
      print(fix$fixid[end])
    } else {
      break
    }
   
  }
  
}
