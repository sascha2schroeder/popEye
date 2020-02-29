
Phase1 <- function(fix, stimmat) {
  
  print("Phase 1")
  
  crit1 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 4
  crit2 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 4
  crit3 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 2
  
  long <- as.numeric(unlist(dimnames(table(fix$run)[table(fix$run) >= 3])))
  old <- length(long) + 1
  new <- length(long)
  
  while (new < old) {
    
    old <- new
    out <- NULL
    
    # outer loop
    for (i in 1:(length(long) - 1)) {
      # i <- 1
      # print(i)
      
      # inner loop
      for (j in (i + 1):length(long)) {
        # j <- i + 1
        # print(j)
        
        tmp <- matrix(NA, 1, 9)
        
        tmp[1, 1] <- long[i]
        tmp[1, 2] <- long[j]
        
        if (sd(fix$yn[fix$run == long[i]]) == 0 | 
            sd(fix$xn[fix$run == long[i]]) == 0 | 
            sd(fix$yn[fix$run == long[j]]) == 0 | 
            sd(fix$xn[fix$run == long[j]]) == 0) {
              next
            } 
            
        # compute regressions
        fm1 <- lm(fix$yn[fix$run == long[i]] ~ scale(fix$xn[fix$run == long[i]]))
        fm2 <- lm(fix$yn[fix$run == long[j]] ~ scale(fix$xn[fix$run == long[j]]))
        fm <- lm(fix$yn[fix$run == long[i] | fix$run == long[j]] ~ 
                   scale(fix$xn[fix$run == long[i] | fix$run == long[j]]))
        tmp[1, 3] <- round(sigma(fm), 3)
        tmp[1, 4] <- round(coef(fm)[1])
        tmp[1, 5] <- round(coef(fm)[2], 3)
        tmp[1, 6] <- round(coef(fm1)[1])
        tmp[1, 7] <- round(coef(fm1)[2], 3)
        tmp[1, 8] <- round(coef(fm2)[1])
        tmp[1, 9] <- round(coef(fm2)[2], 3)
        
        out <- rbind(out, tmp)
        
      }
      
    }
    
    # select candidate
    out2 <- out[order(out[, 3]), ]
    
    if (sum(out2[,3] < crit1 & abs(out2[,5]) < crit2 & abs(out2[,6] - out2[,8]) < crit3) <= 1) {
      break
    }
    
    out3 <- out2[out2[,3] < crit1 & abs(out2[,5]) < crit2 & abs(out2[,6] - out2[,8]) < crit3, ]
    
    cand <- out3[1, ]
    
    # plot
    plot(fix$xn, fix$yn, ylim = c(768, 0), type = "n", main = paste(cand[1], ":", cand[2]))
    points(fix$xn[fix$run == cand[1]], fix$yn[fix$run == cand[1]], col = "green",
           pch = 1, type = "b", cex = 1, lty = 2)
    points(fix$xn[fix$run == cand[2]], fix$yn[fix$run == cand[2]], col = "green",
           pch = 0, type = "b", cex = 1, lty = 3)
    points(fix$xn[fix$run == cand[1] | fix$run == cand[2]], 
           fix$yn[fix$run == cand[1] | fix$run == cand[2]], 
           col = "green", pch = 16, type = "b", cex = 1, lty = 1)
    
    fix$run[fix$run == cand[2]] <- cand[1]
    fix$run <- as.numeric(as.factor(fix$run))
    long <- as.numeric(unlist(dimnames(table(fix$run)[table(fix$run) >= 3])))
    
    new <- length(long)
    
    print(new)
    
  }
  
  return(fix)
  
}
