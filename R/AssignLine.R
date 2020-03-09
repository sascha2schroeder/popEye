
AssignLine <- function(fix, stimmat) {
  
  # message(".... Assign lines")
  
  fix$line <- NA
  
  # # Remove transitions (?)
  # fix <- RemoveTransitions(fix, stimmat)
  # add additional outlier
  
  
  # 1. delete single fixations
  
  # print("Delete single fixations")
  
  while(length(table(fix$linerun)) > length(table(stimmat$line))) {
    
    mrun <- tapply(fix$yn, fix$linerun, mean)
    run <- as.numeric(unlist(dimnames(table(fix$linerun))))
    runs <- table(fix$linerun)
    
    out <- NULL
    for (i in 1:length(mrun)) {
      # i <- 1
      
      if (runs[run[i]] > 1) next 
      
      # out <- NULL
      for (j in 1:length(mrun)) {
        # j <- 1
        
        if (i == j) {
          next
        }
        
        # exclude symmetric elements
        if (is.element(paste(i, j), paste(out[, 2], out[,1]))) {
          next
        }
        
        tmp <- matrix(NA, 1, 3)
        
        tmp[1,1] <- i
        tmp[1,2] <- j
        tmp[1,3] <- (mrun[i]  - mrun[j])^2
        
        # out[j] <- (mrun[i]  - mline[j])^2
        
        out <- rbind(out, tmp)
        
      }
      
    }
    
    if(is.null(out)) {
      break
    }
    
    cand <- out[order(out[,3]), ][1,]
    
    fix$linerun[fix$linerun == cand[1]] <- cand[2]
    fix$linerun <- as.numeric(as.factor(fix$linerun))
    
  }
  
  
  # 2. reduce lineruns to maximum number of lines
  
  # print("Minimize number of lines")
  
  while(length(table(fix$linerun)) > length(table(stimmat$line))) {
    
    mrun <- tapply(fix$yn, fix$linerun, mean)
    
    out <- NULL
    for (i in 1:length(mrun)) {
      # i <- 1
      
      # out <- NULL
      for (j in 1:length(mrun)) {
        # j <- 1
        
        if (i == j) {
          next
        }
        
        # exclude symmetric elements
        if (is.element(paste(i, j), paste(out[, 2], out[,1]))) {
          next
        }
        
        tmp <- matrix(NA, 1, 3)
        
        tmp[1,1] <- i
        tmp[1,2] <- j
        tmp[1,3] <- (mrun[i]  - mrun[j])^2
        
        # out[j] <- (mrun[i]  - mline[j])^2
        
        out <- rbind(out, tmp)
        
      }
      
    }
    
    if(is.null(out)) {
      break
    }
    
    cand <- out[order(out[,3]), ][1,]
    
    fix$linerun[fix$linerun == cand[1]] <- cand[2]
    fix$linerun <- as.numeric(as.factor(fix$linerun))
    
  }
  
  
  # 3. assign to lines by order
  
  # print("Assign lines")
  
  mrun <- tapply(fix$yn, fix$linerun, mean)
  lrun <- as.numeric(unlist(dimnames(mrun[order(mrun)])))
  
  for (i in 1:length(lrun)) {
    # i <- 1
    
    fix$line[fix$linerun == lrun[i]] <- i
    
  }
  
  # # assign short runs by distance
  # 
  # mrun <- tapply(fix$yn[fix$type == "in"], fix$linerun[fix$type == "in"], mean)
  # mline <- tapply(stimmat$ys, stimmat$line, mean) + (tapply(stimmat$ye, stimmat$line, mean) - tapply(stimmat$ys, stimmat$line, mean)) / 2
  # 
  # for (i in 1:length(mrun)) {
  #   # i <- 9
  #   
  #   if (table(fix$linerun)[i] > 2) {
  #     next
  #   }
  #   
  #   out <- NULL
  #   for (j in 1:length(mline)) {
  #     # j <- 1
  #     
  #     out[j] <- (mrun[i]  - mline[j])^2
  #     
  #   }
  #   
  #   fix$line[fix$linerun == i] <- which.min(out)
  #   
  # }
  # 
  # fix$line <- as.numeric(as.factor(fix$line))
  # 
  # 
  # # assign short runs by distance
  # 
  # while(length(table(fix$line)) > length(table(stimmat$line))) {
  #   
  #   mrun <- tapply(fix$yn[fix$type == "in"], fix$line[fix$type == "in"], mean)
  #   mline <- tapply(stimmat$ys, stimmat$line, mean) + (tapply(stimmat$ye, stimmat$line, mean) - tapply(stimmat$ys, stimmat$line, mean)) / 2
  #   
  #   for (i in 1:length(mrun)) {
  #     # i <- 1
  #     
  #     out <- NULL
  #     for (j in 1:length(mline)) {
  #       # j <- 1
  #       
  #       out[j] <- (mrun[i]  - mline[j])^2
  #       
  #     }
  #     
  #     fix$line[fix$linerun == i] <- which.min(out)
  #     
  #   }
  #   
  # }
  
  return(fix)
  
}
