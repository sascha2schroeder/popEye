
DistFromText <- function(tmp, stim, fix) {
  
  coords <- TranslateXY(tmp, fix)
  
  # create text box
  xs <- min(stim$xs); xe <- max(stim$xe)
  ys <- min(stim$ys); ye <- max(stim$ye)
  
  # compute distance matrix
  dist <- matrix(0, nrow(coords), 2)
  
  for (i in 1:nrow(coords)) {
    
    if (coords[i, 1] < xs) {
      dist[i,1] <- coords[i,1] - xs
    }
    
    if (coords[i, 1] > xe) {
      dist[i,1] <- coords[i,1] - xe
    }
    
    if (coords[i, 2] < ys) {
      dist[i,2] <- coords[i,2] - ys
    }
    
    if (coords[i, 2] > ye) {
      dist[i,2] <- coords[i,2] - ye
    }
    
  }
  
  DistClose <- sum(dist[,1]^2 + dist[,2]^2)
  
  return(DistClose)
  
}
