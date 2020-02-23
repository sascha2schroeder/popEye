
Phase4 <- function(fix, stimmat) {
  
  print("Phase 4")
  
  nlines <- max(stimmat$line)
  
  runs <- length(table(fix$run))
  mrun <- tapply(fix$yn, fix$run, mean)
  
  while (runs > nlines) {
  
    cand <- as.numeric(unlist(dimnames(sort(table(fix$run))))[1])
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
    
    fix$run[fix$run == cand] <- dist[which.min(dist[,2]), 1]
    fix$run <- as.numeric(as.factor(fix$run))
    
    mrun <- tapply(fix$yn, fix$run, mean)
    runs <- length(table(fix$run))
    
    print(runs)
    
  }
  
  return(fix)
  
}
