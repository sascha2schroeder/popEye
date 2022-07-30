
PlotAlignSubject <- function(exp, subject) {
  
  ntrial <- exp$reports$subjects$ntrial[exp$reports$subjects$subid == subject]
  trials <- exp$report$trials$itemid[exp$reports$trials$subid == subject]
  
  for (i in 1:ntrial) {
    
    line <- ReadKey()
    
    if (line == "q") {
      break
    }
    
    message(paste("Trial ", i, ": ", trials[i], sep = ""))
    PlotAlign(exp, subject, i)
    
  }
  
}
