
SmoothData <- function(xy, k = 5) {

  # NOTE: caTools package needed.
  # suppressPackageStartupMessages(library(caTools))
  # if ("package:caTools" %in% search() == F){
  #   print("caTools package needed. Please install.")
  # }
  
  # new moving average function
  ma <- function(x, n = k){stats::filter(x, rep(1/n, n), sides = 2, circular = T)}
  
  xys <- data.frame(matrix(data = NA, nrow = nrow(xy), ncol = 3))
  names(xys) <- c('time', 'x',  'y')
  
  xys$time <- xy$time
  
  # xys$x2 <- round(caTools::runmean(xy$x, k = k, align = "center"), 1)
  # xys$y2 <- round(caTools::runmean(xy$y, k = k, align = "center"), 1)
  
  xys$x <- round(ma(xy$x), 1)
  xys$y <- round(ma(xy$y), 1)
  
  # xys <- xys[is.na(xys$x) == F, ]
  
  return(xys)
  
}
