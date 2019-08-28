
ComputeRadius <- function(vxy, env = parent.frame(n = 3)) {
  
  # NOTE: saccade "amplitude" in pixels, not degrees at present
  
  
  # determine tresholds
  # ---------------------
  
  # compute threshold
  medx <- median(vxy$x, na.rm = T)
  msdx <- MedianSD(vxy$x)
  medy <- median(vxy$y, na.rm = T)
  msdy <- MedianSD(vxy$y)
  
  # compute radius
  radiusx <- env$exp$setup$analysis$vfac * msdx
  radiusy <- env$exp$setup$analysis$vfac * msdy
  radius <- data.frame(cbind(radiusx, radiusy))
  names(radius) <- c('x', 'y')
  
  return(radius)
  
}
