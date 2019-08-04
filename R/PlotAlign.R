
PlotAlign <- function(exp, subject, trial, pdf = NULL, interactive = F, 
                      sub = F, align = F, outlier = F) {
  
  # TODO: resize y dimension?
  # TODO: align letters 
  # TODO: make nice
  
  # start pdf
  if (sub == F) {
    if (missing(pdf) == T) {
      par(mfrow = c(1, 1), cex = 1.25, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = T)
    } else {
      pdf(pdf, width = 16, height = 8.5)
      par(mfrow = c(1, 1), cex = .9, oma = c(0, 0, 2, 0))
    }
  }
  
  tmp <- SelectSubjectTrial(exp, subject, trial)
  fix <- tmp$fix
  stimmat <- tmp$meta$stimmat
  
  # basic plot
  if (outlier == F) {
    
    plot(fix$xs, fix$ys, 
         xlim = c(0, exp$setup$display$resolutionX), 
         ylim = c(max(tmp$meta$stimmat$ye) + 1*exp$setup$font$size,
                  min(tmp$meta$stimmat$ys) - 1*exp$setup$font$size),
         type = "n",
         main = paste("Trial", trial), xlab = "x Position (px)", ylab = "y Position (py)")
    
  } else {
    
    plot(fix$xs, fix$ys, 
         xlim = c(0, exp$setup$display$resolutionX), 
         ylim = c(max(fix$ys) + 2*exp$setup$font$size,
                  min(fix$ys) - 1*exp$setup$font$size), 
         type = "n",
         main = paste("Trial", trial), xlab = "x Position (px)", ylab = "y Position (py)")
    
  }
  
  # add letters
  for (i in 1:nrow(stimmat)){
    rect(stimmat$xs[i], stimmat$ye[i], stimmat$xe[i], stimmat$ys[i])  
    text(stimmat$xm[i], stimmat$ym[i], stimmat$letter[i], family = "Courier", cex = .9)
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
  
  # set colors
  if (max(stimmat$line) > 1) {
    palette(topo.colors(max(stimmat$line)))
    # palette(rainbow(max(stimmat$line)))
  } else {
    palette(topo.colors(2))
  }
  
  
  # add fixations
  if (outlier == F) {
    
    inc <- fix[fix$type == "in", ]
    
    if (align == F) {
      
      points(inc$xs,
             inc$ys,
             col = "black",
             pch = 16, type = "l")
      
      points(inc$xs,
             inc$ys,
             col = inc$line,
             pch = 16, type = "p")
      
      for (i in 1:nrow(inc)) {
        text(inc$xs[i], inc$ys[i] - 3, 
             labels = inc$fixid[i],
             col = "black", cex = .75)
      }
      
    } else {
      
      inc$ytmp <- NA
      inc$ytmp <- jitter(inc$ym, .5)
      
      points(inc$xs, inc$ys, cex = .75, 
             type = "p", col = "black", pch = 16)
      points(inc$xn, 
             inc$ytmp, 
             col = "black", 
             pch = 16, type = "l")
      points(inc$xn, 
             inc$ytmp, 
             col = inc$line,
             pch = 16, type = "p")
      arrows(inc$xs, inc$ys, 
             inc$xn, inc$ytmp,
             code = 2, length = .07 )
      
      for (i in 1:nrow(inc)) {
        text(inc$xs[i], inc$ys[i] - 3, 
             labels = inc$fixid[i],
             col = "black", cex = .75)
      }
      
    }
    
  } else {
    
    points(fix$xs,
           fix$ys,
           col = "black",
           pch = 16, type = "l")
    
    points(fix$xs[fix$type == "in"],
           fix$ys[fix$type == "in"],
           col = fix$line[fix$type == "in"],
           pch = 16, type = "p")
    
    # add outlier
    out <- fix[fix$type == "out", ]
    if (nrow(out) > 0) {
      points(out$xs, fix$ys[fix$type == "out"], cex = .75, 
             type = "p", col = "red", pch = 16)
    }

    for (i in 1:nrow(fix)) {
      text(fix$xs[i], fix$ys[i] - 3, 
           labels = fix$fixid[i],
           col = "black", cex = .75)
    }
    
  }
  
  # add blinks
  blink <- tmp$sac[tmp$sac$msg == "BLINK", ]
  if (nrow(blink) > 0) {
    for (i in 1:nrow(blink)) {
      arrows(blink$xs[i], blink$ys[i], blink$xe[i], blink$ye[i], col = "red", code = 0)
    }
  }

  
  # turn off device  
  if (missing(pdf) == F) {
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
