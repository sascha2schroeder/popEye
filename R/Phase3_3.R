
Phase3_3 <- function(fix, stimmat, check = FALSE) {
  
  # print("Phase 3")
  
  crit1 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 4
  crit2 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 4
  
  old <- length(table(fix$linerun)) + 1
  new <- length(table(fix$linerun))
  
  while (new < old) {
    
    if (check == TRUE) {
      ReadKey()
    }
    
    old <- new
    out <- NULL
    
    run <- as.numeric(unlist(dimnames(table(fix$linerun))))
    
    for (i in 1:(length(run) - 1)) {
      # i <- 1
      # print(i)
      
      j <- i + 1
      
      # if (table(fix$linerun)[run[i]] == 1 | table(fix$linerun)[run[j]] == 1) {
      #   next
      # }
      
      tmp <- matrix(NA, 1, 5)
      
      tmp[1, 1] <- run[i]
      tmp[1, 2] <- run[j]
      
      # compute regressions
      fm <- lm(fix$yn[fix$linerun == run[i] | fix$linerun == run[j]] ~ 
                 scale(fix$xn[fix$linerun == run[i] | fix$linerun == run[j]]))
      tmp[1, 3] <- round(sigma(fm))
      tmp[1, 4] <- round(coef(fm)[1])
      tmp[1, 5] <- round(coef(fm)[2])
      
      out <- rbind(out, tmp)
      
    }
    
    if(is.null(out)) {
      next
    }
    
    # select candidate
    out2 <- out[is.na(out[,3]) == F, ]
    out2 <- out2[order(abs(out2[, 3])), , drop = F]
    
    if (sum(out2[,3] < crit1 & abs(out2[,5]) < crit2, na.rm = T) < 1) {
      break
    } else if (sum(out2[,3] < crit1 & abs(out2[,5]) < crit2, na.rm = T) == 1) {
      cand <- out2[out2[,3] < crit1 & abs(out2[,5]) < crit2, ]
    } else if (sum(out2[,3] < crit1 & abs(out2[,5]) < crit2, na.rm = T) > 1) {
      cand <- out2[out2[,3] < crit1 & abs(out2[,5]) < crit2, ][1, ]
    }
    
    # plot
    if (check == TRUE) {
      plot(fix$xn, fix$yn, ylim = c(768, 0), type = "l", main = paste(cand[1], ":", cand[2]))
      points(fix$xn[fix$linerun == cand[1]], fix$yn[fix$linerun == cand[1]], 
             col = "green", pch = 16, type = "b", cex = 1, lty = 1)
      points(fix$xn[fix$linerun == cand[2]], fix$yn[fix$linerun == cand[2]], 
             col = "red", pch = 16, type = "b", cex = 1, lty = 1)
      abline(lm(fix$yn[fix$linerun == cand[1] | fix$linerun == cand[2]] ~ 
                  (fix$xn[fix$linerun == cand[1] | fix$linerun == cand[2]])))
    }
    
    fix$linerun[fix$linerun == cand[2]] <- cand[1]
    fix$linerun <- as.numeric(as.factor(fix$linerun))
    
    new <- length(table(fix$linerun))
    
    # print(new)
    
  }
  
  return(fix)
  
}
