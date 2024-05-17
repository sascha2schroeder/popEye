
SmoothData <- function(xy, k = 5) {

  # new moving average function
  #ma <- function(x, n = k){stats::filter(x, rep(1/n, n), sides = 2, circular = T)}
  ma <- function(x, n = k){stats::filter(x, rep(1/n, n), sides = 2, circular = F)}
  xys <- data.frame(matrix(data = NA, nrow = nrow(xy), ncol = 3))
  names(xys) <- c('time', 'x',  'y')
  
  xys$time <- xy$time
  
  xys$x <- round(ma(xy$x), 1)
  xys$y <- round(ma(xy$y), 1)
  
  return(xys)
  
}
