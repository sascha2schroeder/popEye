
PlotXY <- function(exp, subject, trial) {

  tmp <- SelectSubjectTrial(exp, subject, trial)
 
  # smooth again
  xy2 <- SmoothData(SmoothData(tmp$xy))

  # basic plot
  # plot(xy2$x, xy2$y, type = "l", 
  #      xlim = c(min(tmp$meta$stimmat$xs) - 3*exp$setup$font$size,
  #               max(tmp$meta$stimmat$xs) + 3*exp$setup$font$size),
  #      ylim = c(max(tmp$meta$stimmat$ys) + 3*exp$setup$font$size,
  #               min(tmp$meta$stimmat$ys) - 3*exp$setup$font$size), 
  #      main = "XY Plot", xlab = "x Position (px)", ylab = "y Position (px)")
  
  # full plot
  plot(xy2$x, xy2$y, type = "l",
       xlim = c(1, exp$setup$display$resolutionX),
       ylim = c(exp$setup$display$resolutionY, 1),
       main = "XY Plot", xlab = "x Position (px)", ylab = "y Position (px)")
  
  # add saccades
  sac <- tmp$parse[tmp$parse$msg == "SAC", ]
  for (s in 1:nrow(sac)) {
    j <- sac$start[s]:sac$stop[s]
    lines(xy2$x[j], xy2$y[j], type='l', col='navyblue', lwd = 1)
  }

  # add fixations
  fix <- tmp$parse[tmp$parse$msg == "FIX", ]
  for (s in 1:nrow(fix)) {
    points(fix$xs[s], fix$ys[s], col = 'royalblue', pch = 16, cex = 1)
  }

  # add fixation number
  fix$num <- 1:nrow(fix)
  for (s in 1:nrow(fix)) {
    text(fix$xs[s], fix$ys[s] - 30, fix$num[s], col = 'royalblue', cex = .6)
  }

  # add fixation duration
  fix$dur <- fix$stop - fix$start + 1
  for (s in 1:nrow(fix)) {
    text(fix$xs[s], fix$ys[s] + 30, fix$dur[s], col = 'black', cex = .6)
  }
  
}
