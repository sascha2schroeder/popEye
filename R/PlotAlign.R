
PlotAlign <- function(exp, subject, trial, plot = NULL, interactive = F, 
                      sub = F, align = F, outlier = F, cex = 1) {
  
  # TODO: resize y dimension?
  # TODO: align letters 
  # TODO: make nice
  
  # start pdf
  
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
  fix <- tmp$fix
  stimmat <- tmp$meta$stimmat
  
  
  # basic plot
  plot(fix$xn, fix$yn, 
       xlim = c(0, exp$setup$display$resolutionX), 
       ylim = c(max(tmp$meta$stimmat$ye) + 1 * exp$setup$font$size,
                min(tmp$meta$stimmat$ys) - 1 * exp$setup$font$size),
       type = "n",
       main = paste("Trial ", tmp$meta$itemid, ": ", tmp$meta$itemid, sep = ""), xlab = "x Position (px)", ylab = "y Position (py)")
  
  
  # add letters
  for (i in 1:nrow(stimmat)){
    rect(stimmat$xs[i], stimmat$ye[i], stimmat$xe[i], stimmat$ys[i])  
    text(stimmat$xs[i] + (stimmat$xe[i] - stimmat$xs[i]) / 2, stimmat$ym[i], stimmat$letter[i], family = "Courier", cex = .9)
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
    palette("default")
  } else {
    palette("default")
  }
  
  
  # add fixations
  if (outlier == F) {
    
    inc <- fix[fix$type == "in", ]
    
    if (align == F) {
      
      points(inc$xn,
             inc$yn,
             col = "black",
             pch = 16, type = "l")
      
      points(inc$xn,
             inc$yn,
             col = inc$line,
             pch = 16, type = "p")
      
      for (i in 1:nrow(inc)) {
        text(inc$xn[i], inc$yn[i] - 3, 
             labels = inc$fixid[i],
             col = "black", cex = .75)
      }
      
    } else {
      
      inc$ytmp <- NA
      inc$ytmp <- jitter(inc$ym, .5)
      
      points(inc$xn, inc$yn, cex = .75, 
             type = "p", col = "black", pch = 16)
      points(inc$xn, 
             inc$ytmp, 
             col = "black", 
             pch = 16, type = "l")
      points(inc$xn, 
             inc$ytmp, 
             col = inc$line,
             pch = 16, type = "p")
      arrows(inc$xn, inc$yn, 
             inc$xn, inc$ytmp,
             code = 2, length = .07 )
      
      for (i in 1:nrow(inc)) {
        text(inc$xn[i], inc$yn[i] - 3, 
             labels = inc$fixid[i],
             col = "black", cex = .75)
      }
      
    }
    
  } else {
    
    inc <- fix[fix$type == "in", ]
    
    if (align == F) {
      
      points(inc$xn,
             inc$yn,
             col = "black",
             pch = 16, type = "l")
      
      points(inc$xn,
             inc$yn,
             col = inc$line,
             pch = 16, type = "p")
      
      for (i in 1:nrow(inc)) {
        text(inc$xn[i], inc$yn[i] - 3, 
             labels = inc$fixid[i],
             col = "black", cex = .75)
      }
      
    } else {
      
      inc$ytmp <- NA
      inc$ytmp <- jitter(inc$ym, .5)
      
      points(inc$xn, inc$yn, cex = .75, 
             type = "p", col = "black", pch = 16)
      points(inc$xn, 
             inc$ytmp, 
             col = "black", 
             pch = 16, type = "l")
      points(inc$xn, 
             inc$ytmp, 
             col = inc$line,
             pch = 16, type = "p")
      arrows(inc$xn, inc$yn, 
             inc$xn, inc$ytmp,
             code = 2, length = .07 )
      
      for (i in 1:nrow(inc)) {
        text(inc$xn[i], inc$yn[i] - 3, 
             labels = inc$fixid[i],
             col = "black", cex = .75)
      }
      
    }
    
    # points(fix$xn,
    #        fix$yn,
    #        col = "black",
    #        pch = 16, type = "l")
    # 
    # points(fix$xn[fix$type == "in"],
    #        fix$yn[fix$type == "in"],
    #        col = fix$line[fix$type == "in"],
    #        pch = 16, type = "p")
    
    # add outlier
    out <- fix[fix$type == "out", ]
    if (nrow(out) > 0) {
      points(out$xn, out$yn, cex = .75, 
             type = "p", col = "white", pch = 16)
    }

    # add fixation number
    for (i in 1:nrow(fix)) {
      text(fix$xn[i], fix$yn[i] - 3, 
           labels = fix$fixid[i],
           col = "black", cex = .75)
    }
    
  }
  
  # # TODO: add blinks
  # blink <- tmp$sac[tmp$sac$msg == "BLINK", ]
  # if (nrow(blink) > 0) {
  #   for (i in 1:nrow(blink)) {
  #     arrows(blink$xsn[i], blink$ysn[i], blink$xen[i], blink$yen[i], col = "red", code = 0)
  #   }
  # }
  
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
