
Phase4 <- function(fix, stimmat) {
  
  # print("Phase 4")
  
  nlines <- max(stimmat$line)
  
  runs <- length(table(fix$linerun))
  mrun <- tapply(fix$yn, fix$linerun, mean)
  
  while (runs > nlines) {
  
    cand <- as.numeric(unlist(dimnames(sort(table(fix$linerun))))[1])
    # TODO: do not merge short sequences first
    
    dist <- NULL
    
    for (i in 1:runs) {
      # i <- 1
      # print(i)
      
      tmp <- matrix(NA, 1, 2)
      
      if (i == cand) next
      
      tmp[,1] <- i
      tmp[,2] <- (mrun[cand] - mrun[i])^2
      
      dist <- rbind(dist, tmp)
      
    }
    
    fix$linerun[fix$linerun == cand] <- dist[which.min(dist[,2]), 1]
    fix$linerun <- as.numeric(as.factor(fix$linerun))
    
    mrun <- tapply(fix$yn, fix$linerun, mean)
    runs <- length(table(fix$linerun))
    
    # print(runs)
    
  }
  
  return(fix)
  
}
