
Phase4_2 <- function(fix, stimmat, check = FALSE) {
 
  # message(paste(".... Phase 4"))
  
  crit1 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 2.5
  crit2 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 2.5
  
  old <- length(table(fix$linerun)) + 1
  new <- length(table(fix$linerun))
  
  while (new < old) {
    
    if (check == TRUE) {
      ReadKey()
    }
    
    old <- new
    out <- NULL
    
    run <- as.numeric(unlist(dimnames(table(fix$linerun))))
    
    for (i in 1:length(run)) {
      # i <- 1
      # print(i)
     
      # TODO: necessary?
      # if (sum(fix$linerun == run[i]) == 0) {
      #   next
      # }
      
      # exclude short?
      # if (sum(fix$linerun == run[i]) < 3) {
      #   next
      # }
      
      for (j in 1:length(run)) {
        # j <- i + 1
        # print(j)
        
        # exclude diagonal elements
        if (i == j) {
          next
        }
        
        # exclude symmetric elements
        if (is.element(paste(i, j), paste(out[, 2], out[,1]))) {
          next
        }
        
        # long
        # if (sum(fix$linerun == run[j]) < 3) {
        #   next
        # }
        
        # # exclude single fixations
        # if (table(fix$linerun)[run[i]] == 1 | table(fix$linerun)[run[j]] == 1) {
        #   next
        # }
        
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
    
    if(is.null(out)) {
      next
    }
    
    # select candidate
    out2 <- out[order(abs(out[,5] - out[,6])), , drop = F]
    
    if (sum(abs(out2[,5] - out2[,6]) < crit2) < 1) {
      break
    } else if (sum(abs(out2[,5] - out2[,6]) <= crit2) == 1) {
      cand <- out2[abs(out2[,5] - out2[,6]) <= crit2, ]
    } else if (sum(abs(out2[,5] - out2[,6]) <= crit2) > 1) {
      cand <- out2[abs(out2[,5] - out2[,6]) <= crit2, ][1, ]
    }
    
    # plot
    if (check == TRUE) {
      plot(fix$xn, fix$yn, ylim = c(768, 0), type = "l", main = paste(cand[1], ":", cand[2]))
      points(fix$xn[fix$linerun == cand[1]], fix$yn[fix$linerun == cand[1]], 
             col = "green", pch = 16, type = "b", cex = 1, lty = 1)
      abline(h = cand[5], col = "green")
      points(fix$xn[fix$linerun == cand[2]], fix$yn[fix$linerun == cand[2]], 
             col = "red", pch = 16, type = "b", cex = 1, lty = 1)
      abline(h = cand[6], col = "red")
    }
    
    fix$linerun[fix$linerun == cand[2]] <- cand[1]
    fix$linerun <- as.numeric(as.factor(fix$linerun))
    
    new <- length(table(fix$linerun))
    
    # print(new)
    
  }
  
  return(fix)
  
}
