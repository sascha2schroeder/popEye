
PlotText <- function(exp, subject, trial, pdf = F, interactive = F, sub = F) {
  
  # TODO: resize y dimension?
  # TODO: align letters 
  
  # start pdf
  if (sub == F) {
    if (pdf == T) {
      pdf("Test.pdf", width = 16, height = 8.5)
      par(mfrow = c(1, 1), cex = .9, oma = c(0, 0, 2, 0))
    } else {
      par(mfrow = c(1, 1), cex = 1.25, oma = c(0, 0, 3, 0))
      if (interactive == T) par(ask = T)
    }
  }
  
  tmp <- SelectSubjectTrial(exp, subject, trial)
  
  palette(topo.colors(max(tmp$meta$stimmat$sent)))
  
  # palette(c("red", "blue"))
  
  
  # data
  fix <- tmp$fix
  
  fix$ymtmp <- jitter(fix$ym)

  # basic plot
  plot(fix$xs, fix$ys, 
       ylim = c(max(tmp$meta$stimmat$ye) + 1*exp$setup$font$size,
                min(tmp$meta$stimmat$ys) - 1*exp$setup$font$size), 
       xlim = c(0, exp$setup$display$resolutionX), type = "n", 
       pch = 16, xlab = "x Position (px)", ylab = "y Position (px)", 
       main = paste("Trial", trial, sep = " "))
  
  # add letters
  letters <- tmp$meta$stimmat$letter
  y <- exp$setup$display$marginTop 
  
  for (i in 1:nrow(tmp$meta$stimmat)){
    
    rect(tmp$meta$stimmat$xs[i], tmp$meta$stimmat$ys[i] + 0.5*exp$setup$font$size, 
         tmp$meta$stimmat$xe[i], tmp$meta$stimmat$ye[i] - 0.5*exp$setup$font$size,
         border = NA, col = MakeTransparent(tmp$meta$stimmat$sent[i], alpha = .1))  
    
    text(tmp$meta$stimmat$xm[i], tmp$meta$stimmat$ym[i], 
         tmp$meta$stimmat$letter[i], family = "Courier", cex = .9)
    
  }
  
  # add fixations
  points(fix$xn, fix$ymtmp, type = "l", pch = 16, col = "black")
  for (i in 1:nrow(fix)) {
    points(fix$xn[i], fix$ymtmp[i], type = "p", pch = 16, col = fix$sentnum[i],
           cex = fix$dur[i] / mean(fix$dur))
  }
  
  # # TODO: add blinks
  # blink <- tmp$sac[tmp$sac$msg == "BLINK", ]
  # if (nrow(blink) > 0) {
  #   for (i in 1:nrow(blink)) {
  #     arrows(blink$xs[i], blink$ys[i], blink$xe[i], blink$ye[i], col = "red", code = 0)
  #   }
  # }
  
  # # TODO: add words (?)
  # words <- as.numeric(unlist(dimnames(table(tmp$meta$stimmat$word))))
  # for (j in 1:max(words)) {
  #   rect(min(tmp$meta$stimmat$xs[tmp$meta$stimmat$word == words[j]]),
  #        min(tmp$meta$stimmat$ys[tmp$meta$stimmat$word == words[j]]), 
  #        max(tmp$meta$stimmat$xe[tmp$meta$stimmat$word == words[j]]),
  #        max(tmp$meta$stimmat$ye[tmp$meta$stimmat$word == words[j]]), 
  #        angle = NA, lwd = 2, border = "navyblue")
  # }
  
  # TODO: add sentence boundary
  # sent <- as.numeric(unlist(dimnames(table(tmp$meta$stimmat$sent))))
  # for (j in 1:max(sent)) {
  #   rect(min(tmp$meta$stimmat$xs[tmp$meta$stimmat$sent == sent[j]]),
  #        min(tmp$meta$stimmat$ys[tmp$meta$stimmat$sent == sent[j]]), 
  #        max(tmp$meta$stimmat$xe[tmp$meta$stimmat$sent == sent[j]]),
  #        max(tmp$meta$stimmat$ye[tmp$meta$stimmat$sent == sent[j]]), 
  #        angle = NA, lwd = 2, border = "navyblue")
  # }
  
  # # TODO: add sentence number
  # fix$num <- 1:nrow(fix)
  # for (i in 1:nrow(fix)) {
  #   text(fix$xn[i], (fix$yn[i] - 3), fix$num[i], col = "navyblue", cex = .75)  
  # }
  
  # turn off device  
  if (sub == F) {
    if (pdf == T) {
      # title(paste("Trial", tmp$meta$trialnum, sep = " "), 
            # outer = T, cex.main = 1.75)
      dev.off()
    } else {
      # title(paste("Trial", tmp$meta$trialnum, sep = " "), 
      #       outer = T, cex.main = 2)
      # par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = F)
    }
  }
}
