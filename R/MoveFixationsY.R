
MoveFixationsY <- function(fix, stimmat, env = parent.frame(n = 1)) {
  
  # compute line coordinates
  stimline <- stimmat[duplicated(stimmat$line) == F, c("line", "xs", "xe", "ys", "ye")]
  stimline$xe <- tapply(stimmat$xe, stimmat$line, max)
  
  yrange <- (stimline$ye[stimline$line == 1] - stimline$ys[stimline$line == 1]) / 2
  
  my <- seq(-yrange, yrange, 1)
  
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
  
  env$dat$item[[env$trial]]$meta$move.y <- out[which.max(out[,2]), 1]
  fix$yn[fix$type == "in"] <- fix$yn[fix$type == "in"] + out[which.max(out[,2]), 1]
  
  return(fix)    
  
}
