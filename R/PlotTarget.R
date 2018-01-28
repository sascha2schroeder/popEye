
PlotTarget <- function(exp, subject, trial, pdf = F, interactive = F) {
  
  # start pdf
  if (pdf == T) {
    pdf("Test.pdf", width = 16, height = 8.5)
    par(mfrow = c(1, 2), cex = .9, oma = c(0, 0, 2, 0))
  } else {
    par(mfrow = c(1, 2), cex = 1.1, oma = c(0, 0, 3, 0))
    if (interactive == T) par(ask = T)
  }

  # plots
  PlotTargetX(exp, subject, trial, sub = T)
  PlotTargetTime(exp, subject, trial, sub = T)
  
  # turn off device
  if (pdf == T) {
    title(paste("Trial", SelectSubjectTrial(exp, subject, trial)$meta$trialnum,
                sep = " "), outer = T, cex.main = 1.75)
    dev.off()
  } else {
    title(paste("Trial", SelectSubjectTrial(exp, subject, trial)$meta$trialnum,
                sep = " "), outer = T, cex.main = 2)
    par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
    if (interactive == T) par(ask = F)
  }
  
}
