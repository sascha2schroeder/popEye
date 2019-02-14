
PlotSentenceTime <- function(exp, subject, trial, pdf = F, interactive = F, sub = F) {

  # TODO: Resize y dimension ?
  # TODO: make contingent on number of lines
  
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
  
  # create plot
  plot(tmp$xy$x, type = "l", ylim = c(exp$setup$display$resolutionX, 1),
       xlim = c(-50, tmp$all$start[nrow(tmp$all)]) , main = "Time Plot", xlab = "Time (ms)",
       ylab = "x Position (px)")

  # add start/stop
  abline (v = 0, col = "royalblue", lwd = 2)
  abline (v = tmp$all$start[nrow(tmp$all)], col = "royalblue", lwd = 2)

  # add fixations
  fix <- tmp$fix
  for (i in 1:nrow(fix)){
    lines(fix$start[i]:fix$stop[i],
          rep(fix$xs[i], fix$stop[i] - fix$start[i] + 1),
          col = "royalblue", lwd = 2)
  }

  # add fixation number
  fix$num <- 1:nrow(fix)
  for (i in 1:nrow(fix)) {
    text(((fix$stop[i] + fix$start[i]) / 2), (fix$xs[i] - 25), fix$num[i],
         col = "royalblue", cex = .75)
  }

  # add fixation duration
  for (i in 1:nrow(fix)) {
    text(((fix$stop[i] + fix$start[i]) / 2), (fix$xs[i] + 25), fix$dur[i],
         col = "black", cex = .6)
  }

  # add blinks
  blink <- tmp$all[tmp$all$msg == "BLINK", ]
  if (nrow(blink) > 0){
    for (i in 1:nrow(blink)){
      rect(blink$start[i], 1100, blink$stop[i], -100,
           col= makeTransparent("darkred", alpha = .2))
    }
  }

  # TODO: add information about lines
  # NOTE: Plot letters on y axis does not make sense anymore
  # NOTE: plot lines seperately?
  
  # add lines
  for (i in 1:length(tmp$meta$stimmat$letter)) {
    abline(h = tmp$meta$stimmat$xs[i], cex = .5)
  }
  
  if (max(tmp$meta$stimmat$line) == 1) {
    
    # add letters
    letters <- unlist(strsplit(tmp$meta$stimmat$letter, ""))
    for (i in 1:(length(tmp$meta$stimmat$letter) - 1)) {
      text(-50, (tmp$meta$stimmat$xs[i]  + tmp$meta$stimmat$xe[i + 1]) / 2, letters[i],
           family = exp$setup$font$family, cex = .75)
    }
    
    # add words
    word <- tmp$meta$stimmat[duplicated(tmp$meta$stimmat$word) == F, ]
    for (j in 2:length(word$xe)){
      abline(h = word$xs[j], col = "navyblue", lwd = 2)
    }
    abline(h = min(tmp$meta$stimmat$xs), col = "navyblue", lwd = 2)
    abline(h = max(tmp$meta$stimmat$xe), col = "navyblue", lwd = 2)
  }
  
  # turn off device
  if (sub == F) {
    if (pdf == T) {
      title(paste("Trial", tmp$meta$trialnum,
                  sep = " "), outer = T, cex.main = 1.75)
      dev.off()
    } else {
      title(paste("Trial", tmp$meta$trialnum,
                  sep = " "), outer = T, cex.main = 2)
      par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = F)
    }
  }
}

