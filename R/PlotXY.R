
PlotXY <- function(exp, subject, trial) {

  tmp <- SelectSubjectTrial(exp, subject, trial)
 
  # smooth again
  xy2 <- SmoothData(SmoothData(tmp$xy))

  # basic plot
  plot(xy2$x, xy2$y, type = "l", 
       ylim = c(exp$setup$display$marginY - 2*exp$setup$font$letpix, 
                exp$setup$display$marginY + 2*exp$setup$font$letpix), 
       # xlim = c(exp$setup$display$marginX, 
       #          exp$setup$display$resolutionX),
       xlim = c(exp$setup$display$marginX,
               max(tmp$meta$ia.boundary)*exp$setup$font$letpix),
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
    text(fix$xs[s], fix$ys[s] + 5, fix$num[s], col = 'royalblue', cex = .75)
  }

  # add fixation duration
  fix$dur <- fix$stop - fix$start + 1
  for (s in 1:nrow(fix)) {
    text(fix$xs[s], fix$ys[s] - 5, fix$dur[s], col = 'black', cex = .6)
  }
  
}
