
MedianSD <- function(x) {
  msd <- sqrt(median((x - median(x, na.rm = T))^2, na.rm = T))
  return(msd)
}

