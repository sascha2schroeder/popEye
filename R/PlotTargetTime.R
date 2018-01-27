
# PlotSentenceTime.R
# -------------------

PlotTargetTime <- function(exp, subject, trial, pdf = F, interactive = F, sub = F) {
  
  # trial <- 1
  
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
  
  
  # create plot
  plot(SelectSubject(exp, subject)$trial[[trial]]$xy$x, type = "l", ylim = c(exp$setup$display$resolutionX, 1), 
       xlim = c(-50, nrow(SelectSubject(exp, subject)$trial[[trial]]$xy)),
       main = "Time Plot", xlab = "Time (ms)", ylab = "x Position (px)")
  
  # add start/stop
  abline (v = 0, col = "royalblue", lwd = 2)
  abline (v = nrow(SelectSubject(exp, subject)$trial[[trial]]$xy), col = "royalblue", lwd = 2)
  
  # add fixations
  fix <- SelectSubject(exp, subject)$trial[[trial]]$fix
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
  blink <- SelectSubject(exp, subject)$trial[[trial]]$all[SelectSubject(exp, subject)$trial[[trial]]$all$msg == "BLINK", ]
  if (nrow(blink) > 0){
    for (i in 1:nrow(blink)){
      rect(blink$start[i], 1100, blink$stop[i], -100,
           col= makeTransparent("darkred", alpha = .2))
    }
  }
  
  # add text
  words <- gsub("\\*", " ", SelectSubject(exp, subject)$trial[[trial]]$stim$text)
  letters <- unlist(strsplit(words, ""))
  
  # variables
  x <- exp$setup$display$marginX
  
  # add lines
  lin = seq(x, x + nchar(words) * exp$setup$font$letpix, by = exp$setup$font$letpix)
  for (i in 1:length(lin)) {
    abline(h = lin[i], cex = .5)
  }
  
  # add letters
  for (i in 1:(length(lin) - 1)) {
    text(-50, lin[i]  + exp$setup$font$letpix / 2, letters[i], 
         family = exp$setup$font$family, cex = .75)
  }
  
  # add words
  let <- sapply(unlist(strsplit(words, " ")), nchar)
  abline(h = x, col = "navyblue", lwd = 2)
  abline(h = x  + let[1] * exp$setup$fon$letpix, col = "navyblue", lwd = 2)
  for (j in 2:length(let)){
    abline(h = x + (sum(let[1:j]) + (j - 1)) * exp$setup$fon$letpix, col = "navyblue", lwd = 2)
  }
  
  # add target word
  j <- SelectSubject(exp, subject)$trial[[trial]]$stim$target
  rect(-200, x + (sum(let[1:(j - 1)]) + (j - 2)) * exp$setup$fon$letpix, 
       nrow(SelectSubject(exp, subject)$trial[[trial]]$xy), x + (sum(let[1:j]) + (j - 1)) * exp$setup$fon$letpix,  
       angle = NA, lwd = 2, border = "navyblue", col = makeTransparent("navyblue", alpha = .2))
  
  # turn off device  
  if (sub == F) {
    if (pdf == T) {
      title(paste("Trial", SelectSubject(exp, subject)$trial[[trial]]$meta$trialnum,
                  sep = " "), outer = T, cex.main = 1.75)
      dev.off()
    } else {
      title(paste("Trial", SelectSubject(exp, subject)$trial[[trial]]$meta$trialnum, 
                  sep = " "), outer = T, cex.main = 2)
      par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = F)
    }  
  }
}
