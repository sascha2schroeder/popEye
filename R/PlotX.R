
PlotX <- function(exp, subject, trial) {
  
  tmp <- SelectSubjectTrial(exp, subject, trial)
  
  # smooth again
  xy2 <- SmoothData(SmoothData(tmp$xy))
  
  # create plot
  plot(xy2$x, type = "l", 
       ylim = c(max(tmp$meta$stimmat$xs) + 3*exp$setup$font$size,
                min(tmp$meta$stimmat$xs) - 3*exp$setup$font$size),
       main = "X Plot", 
       xlab = "Time (ms)", ylab = "x Position (px)")
  
  # add start/stop
  abline (v = 0, col = "navyblue", lwd = 2)
  abline (v = nrow(tmp$xy), col = "navyblue", lwd = 2)
  
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
           col= makeTransparent("darkred", alpha = .2))
    }    
  }

  # TODO: add character lines etc. ?
  # NOTE: maybe in later plot routine including linguistic information
  
}