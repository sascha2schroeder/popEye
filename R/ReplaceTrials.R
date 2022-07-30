
ReplaceTrials <- function(exp1, exp2) {
  
  sub <- exp2$reports$subjects$subid
  sublength <- length(sub)
  
  for (s in 1:sublength) {
    
    trials <- exp2$reports$trials$itemid[exp2$reports$trials$subid == sub[s]]
    
    exp <- DeleteTrials(exp1, sub[s], trials)
    exp <- AddTrials(exp, exp2, sub[s])
    
  }
  
  return(exp)
  
}
