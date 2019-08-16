
PlotSentenceX <- function(exp, subject, trial, pdf = NULL, interactive = F, 
                          sub = F, cex = 1) {
  
  # TODO: resize y dimension?
  # TODO: align letters 
  
  # start pdf
  if (sub == F) {
    if (missing(pdf) == T) {
      par(mfrow = c(1, 1), cex = cex, oma = c(0, 0, 3, 0))
      if (interactive == T) par(ask = T)
    } else {
      pdf(pdf, width = 16, height = 8.5)
      par(mfrow = c(1, 1), cex = cex, oma = c(0, 0, 2, 0))
    }
  }
  
  tmp <- SelectSubjectTrial(exp, subject, trial)
  
  # data
  fix <- tmp$fix

  # basic plot
  plot(fix$xs, fix$ys, 
       ylim = c(max(tmp$meta$stimmat$ye) + 1*exp$setup$font$size,
                min(tmp$meta$stimmat$ys) - 1*exp$setup$font$size), 
       xlim = c(0, exp$setup$display$resolutionX), type = "n", 
       pch = 16, xlab = "x Position (px)", ylab = "y Position (px)", 
       main = "X Plot")
  
  # add letters
  letters <- tmp$meta$stimmat$letter
  y <- exp$setup$display$marginTop 
  
  for (i in 1:nrow(tmp$meta$stimmat)){
    rect(tmp$meta$stimmat$xs[i], tmp$meta$stimmat$ye[i], 
         tmp$meta$stimmat$xe[i], tmp$meta$stimmat$ys[i])  
    
    rect(tmp$meta$stimmat$xs[i], tmp$meta$stimmat$ys[i] + 0.5*exp$setup$font$size, 
         tmp$meta$stimmat$xe[i], tmp$meta$stimmat$ye[i] - 0.5*exp$setup$font$size,
         border = "navyblue", col = makeTransparent("cornflowerblue", alpha = .1))  
    
    text(tmp$meta$stimmat$xm[i], tmp$meta$stimmat$ym[i], 
         tmp$meta$stimmat$letter[i], family = "Courier", cex = .9)
  }
  
  # add fixations
  points(fix$xn, fix$yn, type = "b", pch = 16, col = "royalblue", 
         cex = fix$dur / mean(fix$dur))
  
  # add blinks
  blink <- tmp$sac[tmp$sac$msg == "BLINK", ]
  
  if (nrow(blink) > 0) {
    for (i in 1:nrow(blink)) {
      arrows(blink$xs[i], blink$ys[i], blink$xe[i], blink$ye[i], col = "red", code = 0)
    }
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
  
  # add fixation number
  fix$num <- 1:nrow(fix)
  for (i in 1:nrow(fix)) {
    text(fix$xn[i], (fix$yn[i] - 3), fix$num[i], col = "navyblue", cex = .75)  
  }
  
  # turn off device  
  if (sub == F) {
    if (missing(pdf) == T) {
      title(paste("Trial", tmp$meta$trialid, sep = " "), 
            outer = T, cex.main = 2)
      par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = F)
    } else {
      title(paste("Trial", tmp$meta$trialid, sep = " "), 
            outer = T, cex.main = 1.75)
      dev.off()
    }
  }
}
