
Phase2 <- function(fix, stimmat) {
  
  print("Phase 2")
  
  crit1 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 4
  crit2 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 2
  
  old <- length(table(fix$run)) + 1
  new <- length(table(fix$run))
  
  while (new < old) {
    
    old <- new
    out <- NULL
    
    long <- as.numeric(unlist(dimnames(table(fix$run)[table(fix$run) >= 3])))
    run <- as.numeric(unlist(dimnames(table(fix$run))))
    
    # outer loop
    for (i in 1:length(long)) {
      # i <- 1
      # print(paste("Outer: ", i))
      
      # inner loop
      for (j in 1:length(table(fix$run))) {
        # j <- i + 1
        # print(paste("Inner: ", j))
        
        if (long[i] == run[j]) {
          next
        }
        
        tmp <- matrix(NA, 1, 6)
        
        tmp[1, 1] <- long[i]
        tmp[1, 2] <- run[j]
        
        # compute regressions
        fm1 <- lm(fix$yn[fix$run == long[i]] ~ 1)
        fm2 <- lm(fix$yn[fix$run == run[j]] ~ 1)
        fm <- lm(fix$yn[fix$run == long[i] | fix$run == run[j]] ~ 1)
        tmp[1, 3] <- round(sigma(fm), 3)
        tmp[1, 4] <- round(coef(fm)[1])
        tmp[1, 5] <- round(coef(fm1)[1])
        tmp[1, 6] <- round(coef(fm2)[1])
        
        out <- rbind(out, tmp)
        
      }
      
    }
    
    # select candidate
    out2 <- out[order(out[, 3]), ]
    
    if (sum(out2[,3] < crit1 & abs(out2[,5] - out2[,6]) < crit2) <= 1) {
      break
    }
    
    out3 <- out2[out2[,3] < crit1 & abs(out2[,5] - out2[,6]) < crit2, ]
    
    cand <- out3[1, ]
  
    fix$run[fix$run == cand[2]] <- cand[1]
    fix$run <- as.numeric(as.factor(fix$run))
    
    new <- length(table(fix$run))
    
    print(new)
    
  }
  
  return(fix)
  
}
