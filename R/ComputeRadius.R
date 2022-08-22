
ComputeRadius <- function(vxy, env = parent.frame(n = 4)) {
  
  # NOTE: saccade "amplitude" in pixels, not degrees at present
  
  # determine tresholds
  # ---------------------
  
  # compute threshold
  msdx <- MedianSD(vxy$x)
  msdy <- MedianSD(vxy$y)
  
  # compute radius
  x <- env$exp$setup$analysis$vfac * msdx
  y <- env$exp$setup$analysis$vfac * msdy
  radius <- data.frame(cbind(x, y))
  
  return(radius)
  
}
