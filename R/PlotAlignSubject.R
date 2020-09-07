
PlotAlignSubject <- function(exp, subject) {
  
  ntrial <- exp$out$subjects$ntrial[exp$out$subjects$subid == subject]
  trials <- exp$out$trials$itemid[exp$out$trials$subid == subject]
  
  for (i in 1:ntrial) {
    
    line <- ReadKey()
    
    if (line == "q") {
      break
    }
    
    message(paste("Trial ", i, ": ", trials[i], sep = ""))
    PlotAlign(exp, subject, i)
    
  }
  
}
