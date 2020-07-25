
PlotStimulus <- function(exp, subject, trial, plot = NULL, interactive = F, sub = F,
                         cex = 1) {
  
  # TODO: resize y dimension?
  # TODO: align letters 
  # TODO: make nice
  
  # start plot
  if (sub == F) {
    if (missing(plot) == T) {
      par(mfrow = c(1, 1), cex = cex, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = T)
    } else {
      tmp <- unlist(strsplit(plot, "\\."))
      if (tmp[length(tmp)] == "pdf") {
        pdf(plot, width = 16, height = 8.5)
        par(mfrow = c(1, 1), cex = cex, oma = c(1, 0, 2, 0))
      } else if (tmp[length(tmp)] == "png") {
        png(plot, width = 2000, height = 1000)
        par(mfrow = c(1, 1), cex = cex, oma = c(1, 0, 2, 0))  
      }
    }
  }
  
  tmp <- SelectSubjectTrial(exp, subject, trial)
  stimmat <- tmp$meta$stimmat
  
  # basic plot
  plot(stimmat$xs, stimmat$ys, 
       xlim = c(0, exp$setup$display$resolutionX), 
       ylim = c(max(tmp$meta$stimmat$ye) + 1*exp$setup$font$size,
                min(tmp$meta$stimmat$ys) - 1*exp$setup$font$size), 
       type = "n",
       main = paste("Trial", trial), xlab = "x Position (px)", ylab = "y Position (py)")
  
  # add letters
  letters <- tmp$meta$stimmat$letter
  y <- exp$setup$display$marginTop 
  
  for (i in 1:nrow(tmp$meta$stimmat)){
    rect(tmp$meta$stimmat$xs[i], tmp$meta$stimmat$ye[i], 
         tmp$meta$stimmat$xe[i], tmp$meta$stimmat$ys[i])  
    
    rect(tmp$meta$stimmat$xs[i], tmp$meta$stimmat$ys[i] + 0.5*exp$setup$font$size, 
         tmp$meta$stimmat$xe[i], tmp$meta$stimmat$ye[i] - 0.5*exp$setup$font$size,
         border = "navyblue", col = MakeTransparent("cornflowerblue", alpha = .1))  
    
    text(tmp$meta$stimmat$xm[i], tmp$meta$stimmat$ym[i], 
         tmp$meta$stimmat$letter[i], family = "Courier", cex = .9)
  }

  # add words
  words <- as.numeric(unlist(dimnames(table(tmp$meta$stimmat$wordnum))))
  for (j in 1:max(words)) {
    rect(min(tmp$meta$stimmat$xs[tmp$meta$stimmat$wordnum == words[j]]),
         min(tmp$meta$stimmat$ys[tmp$meta$stimmat$wordnum == words[j]]), 
         max(tmp$meta$stimmat$xe[tmp$meta$stimmat$wordnum == words[j]]),
         max(tmp$meta$stimmat$ye[tmp$meta$stimmat$wordnum == words[j]]), 
         angle = NA, lwd = 2, border = "navyblue")
  }
  
  # turn off device  
  if (missing(plot) == F) {
    # title(paste("Trial", SelectSubjectTrial(exp, subject, trial)$meta$trialnum, 
                # sep = " "), outer = T, cex.main = 3.75)
    dev.off()
  } else {
    # title(paste("Trial", SelectSubjectTrial(exp, subject, trial)$meta$trialnum, 
                # sep = " "), outer = T, cex.main = 2)
    par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
    if (interactive == T) par(ask = F)
  }
  
}
