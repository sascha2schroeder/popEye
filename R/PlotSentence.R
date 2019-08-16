
PlotSentence <- function(exp, subject, trial, pdf = NULL, interactive = F, 
                         cex = 1) {
  
  # start pdf
  if (missing(pdf) == T) {
    par(mfrow = c(1, 2), cex = cex, oma = c(0, 0, 3, 0))
    if (interactive == T) par(ask = T)
  } else {
    pdf(pdf, width = 16, height = 8.5)
    par(mfrow = c(1, 2), cex = cex, oma = c(0, 0, 2, 0))
  }

  # plots
  PlotSentenceX(exp, subject, trial, sub = T, cex = cex)
  PlotSentenceTime(exp, subject, trial, sub = T, cex = cex)
  
  # turn off device  
  if (missing(pdf) == T) {
    title(paste("Trial", SelectSubjectTrial(exp, subject, trial)$meta$trialid, 
                sep = " "), outer = T, cex.main = 2)
    par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
    if (interactive == T) par(ask = F)
  } else {
    title(paste("Trial", SelectSubjectTrial(exp, subject, trial)$meta$trialid, 
                sep = " "), outer = T, cex.main = 1.75)
    dev.off()
  }
  
}
