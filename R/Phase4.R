
Phase4 <- function(fix, stimmat, check = FALSE) {
  
  # message(paste(".... Phase 4"))
  
  crit1 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 1.5
  
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
      
      for (j in 1:length(run)) {
        # j <- 2
        
        if (i == j) {
          next
        }
        
        tmp <- matrix(NA, 1, 3)
        tmp[1,1] <- i
        tmp[1,2] <- j
        
        fix1 <- fix[is.na(fix$linerun) == F & fix$linerun == run[i], ]
        fix2 <- fix[is.na(fix$linerun) == F & fix$linerun == run[j], ]
        
        outtmp1 <- NULL
        for (k in 1:nrow(fix1)) {
          # k <- 1
          
          outtmp2 <- NULL
          for (l in 1:nrow(fix2)) {
            # l <- 1
            
            outtmp2[l] <- sqrt((fix1$xn[k] - fix2$xn[l])^2 + (fix1$yn[k] - fix2$yn[l])^2)
            
          }
          
          outtmp1[k] <- min(outtmp2)
          
        }
        
        tmp[1,3] <- mean(outtmp1)
        out <- rbind(out, tmp)
        
      }
      
    }
    
    if(is.null(out)) {
      next
    }
    
    # select candidate
    out2 <- out[order(out[, 3]), , drop = F]
    
    if (sum(out2[,3] < crit1) < 1) {
      break
    } else if (sum(out2[,3] < crit1) == 1) {
      cand <- out2[out2[,3] < crit1, ]
    } else if (sum(out2[,3] < crit1) > 1) {
      out3 <- out2[out2[,3] < crit1, ]
      cand <- out3[1, ]
    }
    
    # plot
    if (check == TRUE) {
      plot(fix$xn, fix$yn, ylim = c(768, 0), type = "l", main = paste(cand[1], ":", cand[2]))
      points(fix$xn[fix$linerun == cand[1]], fix$yn[fix$linerun == cand[1]], 
             col = "green", pch = 16, type = "b", cex = 1, lty = 1)
      points(fix$xn[fix$linerun == cand[2]], fix$yn[fix$linerun == cand[2]], 
             col = "red", pch = 16, type = "b", cex = 1, lty = 1)
    }
    
    fix$linerun[fix$linerun == cand[1]] <- cand[2]
    fix$linerun <- as.numeric(as.factor(fix$linerun))
    
    new <- length(table(fix$linerun))
    
    # print(new)
    
  }
  
  return(fix)
  
}
