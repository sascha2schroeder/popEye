
PlotFixations <- function(exp, subject, trial, start = 1, stop = NA,
                          pdf = NULL, interactive = T) {
  
  fix <- SelectSubjectTrial(exp, subject, trial)$fix
  
  # set colors
  if (max(fix$line, na.rm = T) > 1) {
    palette("default")
    # palette(topo.colors(max(fix$line)))
    # palette(rainbow(max(fix$line, na.rm = T)))
  } else {
    # palette(topo.colors(2))
    palette("default")
  }
  
  
  if (missing(stop)) {
    stop <- max(fix$fixid)
  } 
  
  end <- start
  
  while(end <= stop) {
    
    # start pdf
    if (missing(pdf) == T) {
      par(mfrow = c(1, 1), cex = 1.25, oma = c(0, 0, 0, 0))
    } else {
      pdf(pdf, width = 16, height = 8.5)
      par(mfrow = c(1, 1), cex = .9, oma = c(0, 0, 2, 0))
    }
    
    PlotStimulus(exp, subject, trial)
  
    points(fix$xn[start:end],
           fix$yn[start:end],
           col = "black",
           pch = 16, type = "l")
    
    # if (fix$type[end:stop] == "out") {
    #   points(fix$xs[end:stop],
    #          fix$ys[end:stop],
    #          col = "black",
    #          pch = 16, cex = 1, type = "p")
    # }
    
    fix$line[fix$type == "out"] <- 0
    
      
      points(fix$xn[start:end],
             fix$yn[start:end],
             bg = fix$line[start:end],
             pch = 21, cex = 1, type = "p")
      
      points(fix$xn[end],
             fix$yn[end],
             bg = fix$line[end],
             pch = 21, cex = 2, type = "p")
      
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
