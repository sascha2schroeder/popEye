
SmallestBox <- function(fix) {
  
  lx = min(fix[,1])
  ux = max(fix[,1])
  ly = min(fix[,2])
  uy = max(fix[,2])
  
  points <- c(lx, ux, ly, uy)
  area <- (ux - lx) * (uy - ly)
  
  out <- list(points = points, area = area)
  return(out)
  
}
