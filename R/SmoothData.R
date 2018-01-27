
SmoothData <- function(xy, k = 5) {

  # NOTE: caTools package needed.
  
  suppressPackageStartupMessages(library(caTools))
  
  if ("package:caTools" %in% search() == F){
    print("caTools package needed. Please install.")
  }
  
  xys <- data.frame(matrix(data = NA, nrow = nrow(xy), ncol = 3))
  names(xys) <- c('time', 'x',  'y')
  
  xys$time <- xy$time
  xys$x <- round(caTools::runmean(xy$x, k = k, align = "center"), 1)
  xys$y <- round(caTools::runmean(xy$y, k = k, align = "center"), 1)
  
  return(xys)
  
}
