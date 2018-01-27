
PlotVelocity <- function(exp, subject, trial) {

  tmp <- SelectSubjectTrial(exp, subject, trial)
  
  # create basic plot
  plot(tmp$vxy$x, tmp$vxy$y, type = "n", main = "2D Velocity Plot",
       xlab = "x Velocity", ylab = "y Velocity", ylim = rev(range(tmp$vxy$y, na.rm = T)))
  
  points(tmp$vxy$x, tmp$vxy$y, type = "l", col = "royalblue", lwd = .5)
  
  # compute radius
  rad = unlist(ComputeSaccades(tmp$xy, tmp$vxy)$radius)
  
  # kernel
  phi <- as.vector(seq(from = 0, to = 2*pi, length.out = 300))
  cx <- rad[1]*matrix(cos(phi))
  cy <- rad[2]*sin(phi)
  lines(cx, cy, col = "red", lwd = 2, lty = 1)
  
  # add saccades
  sac <- tmp$parse[tmp$parse$msg == "SAC", ]
  for (s in 1:nrow(sac)) {
    j <- sac$start[s]:sac$stop[s]
    lines(tmp$vxy$x[j], tmp$vxy$y[j], type = 'p', pch = 16, cex = .5, col = 'navyblue', 
          lty = 2)
  }
  
}
