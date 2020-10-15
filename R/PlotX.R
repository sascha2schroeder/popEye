
PlotX <- function(exp, subject, trial, sub = F, pdf = NULL, interactive = F) {
  
  # start pdf
  if (sub == F) {
    if (missing(pdf) == T) {
      par(mfrow = c(1, 1), cex = 1.25, oma = c(0, 0, 3, 0))
      if (interactive == T) par(ask = T)
    } else {
      pdf(pdf, width = 16, height = 8.5)
      par(mfrow = c(1, 1), cex = .9, oma = c(0, 0, 2, 0))
    }
  }
  
  tmp <- SelectSubjectTrial(exp, subject, trial)
  
  # smooth again
  xy2 <- SmoothData(SmoothData(tmp$xy))
  
  # create plot
  plot(xy2$time, xy2$x, type = "l", 
       ylim = c(max(tmp$meta$stimmat$xs) + 3*exp$setup$font$size,
                min(tmp$meta$stimmat$xs) - 3*exp$setup$font$size),
       main = "X Plot", 
       xlab = "Time (ms)", ylab = "x Position (px)")
  
  # add start/stop
  abline (v = 0, col = "navyblue", lwd = 2)
  abline (v = max(tmp$xy$time), col = "navyblue", lwd = 2)
  
  # add start saccade
  sac <- tmp$parse[tmp$parse$msg == "SAC", ]
  for (i in 1:nrow(sac)){
    abline(v = sac$start, col = "navyblue", lwd = 1, lty = 1)
  }
  
  # add end saccade
  for (i in 1:nrow(sac)){
    abline(v = sac$stop, col = "navyblue", lwd = 1, lty = 2)
  }
  
  # add fixations
  fix <- tmp$parse[tmp$parse$msg == "FIX", ]
  for (i in 1:nrow(fix)){
    lines(fix[i, 2]:fix[i, 3], 
          rep(fix$xs[i], fix$stop[i] - fix$start[i] + 1), 
          col = "royalblue", lwd = 2)  
  }
  
  # add fixation number
  fix$num <- 1:nrow(fix)
  for (i in 1:nrow(fix)) {
    text(((fix$stop[i] + fix$start[i]) / 2), (fix$xs[i] - 30), fix$num[i], 
         col = "royalblue", cex = .6)  
  }
  
  # add fixation duration
  fix$dur <- fix$stop - fix$start + 1
  for (i in 1:nrow(fix)) {
    text(((fix$stop[i] + fix$start[i]) / 2), (fix$xs[i] + 30), fix$dur[i], 
         col = "black", cex = .6)  
  }
  
  # TODO: add distance ?
  
  # add blinks
  blink <- tmp$parse[tmp$parse$msg == "BLINK", ]
  if (nrow(blink) > 0){
    for (i in 1:nrow(blink)){
      rect(blink$start[i], 1100, blink$stop[i], -100, 
           col= MakeTransparent("darkred", alpha = .2))
    }    
  }

  # TODO: add character lines etc. ?
  # NOTE: maybe in later plot routine including linguistic information
  
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
