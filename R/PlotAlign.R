
PlotAlign <- function(exp, subject, trial, pdf = NULL, interactive = F, sub = F, align = F) {
  
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
  plot(fix$xs, fix$ys, 
       xlim = c(0, exp$setup$display$resolutionX), 
       ylim = c(max(tmp$meta$stimmat$ye) + 1*exp$setup$font$size,
                min(tmp$meta$stimmat$ys) - 1*exp$setup$font$size), 
       type = "n",
       main = paste("Trial", trial), xlab = "x Position (px)", ylab = "y Position (py)")
  
  # add letters
  for (i in 1:nrow(stimmat)){
    rect(stimmat$xs[i], stimmat$ye[i], stimmat$xe[i], stimmat$ys[i])  
    text(stimmat$xm[i], stimmat$ym[i], stimmat$letter[i], family = "Courier", cex = .9)
  }
  
  # original and corrected fixations
  
  if (max(stimmat$line) > 1) {
    palette(topo.colors(max(stimmat$line)))
  } 
  
  if (align == F) {
    # points(fix$xs[fix$type == "in"], fix$ys[fix$type == "in"], cex = .75, 
    #        type = "p", col = "black", pch = 16)
      points(fix$xn[fix$type == "in"],
           fix$yn[fix$type == "in"],
           col = "black",
           pch = 16, type = "b")
    # arrows(fix$xs[fix$type == "in"], fix$ys[fix$type == "in"], 
    #        fix$xn[fix$type == "in"], fix$yn[fix$type == "in"],
    #        code = 2, length = .07 )
    
  } else {
    
    fix$ytmp <- NA
    fix$ytmp[fix$type == "in"] <- jitter(fix$ym[fix$type == "in"], .5)
    
    points(fix$xs[fix$type == "in"], fix$ys[fix$type == "in"], cex = .75, 
           type = "p", col = "black", pch = 16)
    points(fix$xn[fix$type == "in"], 
           fix$ytmp[fix$type == "in"], 
           col = fix$line[fix$type == "in"], 
           pch = 16, type = "p")
    points(fix$xn[fix$type == "in"], 
           fix$ytmp[fix$type == "in"], 
           col = "black", 
           pch = 16, type = "l")
    arrows(fix$xs[fix$type == "in"], fix$ys[fix$type == "in"], 
           fix$xn[fix$type == "in"], fix$ytmp[fix$type == "in"],
           code = 2, length = .07 )
    
  }
  
  # add fixation number
  for (i in 1:nrow(fix)) {
    text(fix$xn[i], fix$yn[i] - 3, labels = fix$fixid[i],
         col = "black", cex = .75)
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
