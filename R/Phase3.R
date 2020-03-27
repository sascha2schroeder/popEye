
Phase3 <- function(fix, stimmat) {
  
  # print("Phase 3")
  
  crit1 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 4
  crit2 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 2
  
  old <- length(table(fix$linerun)) + 1
  new <- length(table(fix$linerun))
  
  while (new < old) {
    
    old <- new
    out <- NULL
    
    run <- as.numeric(unlist(dimnames(table(fix$linerun))))
    
    # outer loop
    for (i in 1:length(run)) {
      # i <- 1
      # print(i)
    
      if (sum(fix$linerun == run[i]) == 0) {
        next
      }
      
      # inner loop
      for (j in 1:length(run)) {
        # j <- i + 1
        # print(j)
        
        if (i == j) {
          next
        }
        
        if (sum(fix$linerun == run[j]) == 0) {
          next
        }
        
        tmp <- matrix(NA, 1, 6)
        
        tmp[1, 1] <- run[i]
        tmp[1, 2] <- run[j]
        
        # compute regressions
        fm1 <- lm(fix$yn[fix$linerun == run[i]] ~ 1)
        fm2 <- lm(fix$yn[fix$linerun == run[j]] ~ 1)
        fm <- lm(fix$yn[fix$linerun == run[i] | fix$linerun == run[j]] ~ 1)
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
    
    fix$linerun[fix$linerun == cand[2]] <- cand[1]
    fix$linerun <- as.numeric(as.factor(fix$linerun))
    
    new <- length(table(fix$linerun))
    
    # print(new)
    
  }
  
  return(fix)
  
}
