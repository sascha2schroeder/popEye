
PlotVelocity <- function(exp, subject, trial, sub = F, pdf = pdf) {
  
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
  
  # create basic plot
  plot(tmp$vxy$x, tmp$vxy$y, type = "n", main = "Velocity Plot",
       xlab = "x Velocity", ylab = "y Velocity", 
       ylim = rev(range(tmp$vxy$y, na.rm = T)))
  
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
