
MoveFixations2 <- function(fix, stimmat) {
  
  # compute line coordinates
  stimline <- stimmat[duplicated(stimmat$line) == F, c("line", "xs", "xe", "ys", "ye")]
  stimline$xe <- tapply(stimmat$xe, stimmat$line, max)
  
  
  # optimize x
  # -----------
  
  mx <- seq(-50, 50, 1)
  
  out <- matrix(NA, length(mx), 2)
  
  for (k in 1:length(mx)) {
    
    out[k, 1] <- mx[k]
    
    hit <- 0
    for (i in 1:nrow(fix[fix$type == "in", ])) {
      # i <- 1
      
      for (j in 1:nrow(stimline)) {
        # j <- 1
        
        if (fix$xn[fix$type == "in"][i] + mx[k] > stimline$xs[j] & 
            fix$xn[fix$type == "in"][i] + mx[k] < stimline$xe[j] &
            fix$yn[fix$type == "in"][i] > stimline$ys[j] &
            fix$yn[fix$type == "in"][i] < stimline$ye[j]) {
          hit <- hit + 1
        }
        
      }
      
      out[k, 2] <- hit
      
    }
    
  }
  
  fix$xn[fix$type == "in"] <- fix$xn[fix$type == "in"] + out[which.max(out[,2]), 1]
  
  
  
  # optimize y
  # -----------
  
  my <- seq(-50, 50, 1)
  
  out <- matrix(NA, length(my), 2)
  
  for (k in 1:length(my)) {
    
    out[k, 1] <- my[k]
    
    hit <- 0
    for (i in 1:nrow(fix[fix$type == "in", ])) {
      # i <- 1
      
      for (j in 1:nrow(stimline)) {
        # j <- 1
        
        if (fix$xn[fix$type == "in"][i] > stimline$xs[j] & 
            fix$xn[fix$type == "in"][i] < stimline$xe[j] &
            fix$yn[fix$type == "in"][i] + my[k] > stimline$ys[j] &
            fix$yn[fix$type == "in"][i] + my[k] < stimline$ye[j]) {
          hit <- hit + 1
        }
        
      }
      
      out[k, 2] <- hit
      
    }
    
  }
  
  fix$yn[fix$type == "in"] <- fix$yn[fix$type == "in"] + out[which.max(out[,2]), 1]
  
  return(fix)    
  
}
