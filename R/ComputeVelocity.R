
ComputeVelocity <- function(xy, type = 2, env = parent.frame(n = 2)) {

  # retrieve sample rate
  samp <- as.numeric(env$exp$setup$tracker$samp)
  
  # prep
  N <- nrow(xy)
  v <- data.frame(matrix(data = 0, nrow = N, ncol = 3)) 
  colnames(v) <- c('time', 'x',  'y')
  
  v$time <- xy$time

  # compute velocity
  if (type == 1) {
    # TYPE 1 algorithm
    v[2:(N - 1), 2:3] = samp / 2 * (xy[3:N, 2:3] - xy[1:(N - 2), 2:3])
  }

  if (type == 2) {
    # TYPE 2 algorithm
    v[3:(N - 2), 2:3] = samp / 6 * (xy[5:N, 2:3] + xy[4:(N - 1), 2:3] 
                                    - xy[2:(N - 3), 2:3] - xy[1:(N - 4), 2:3])
    v[2, 2:3] = samp / 2 * (xy[3, 2:3] - xy[1, 2:3])
    v[(N - 1), 2:3] = samp / 2 * (xy[N, 2:3] - xy[N-2, 2:3])
  }
 
  v$x <- round(v$x, 1)
  v$y <- round(v$y, 1)
  
  return(v)

}