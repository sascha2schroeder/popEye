
ComputeSaccades <- function(xy, vxy, env = parent.frame(n = 3)) {
  
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
  
  # compute test criterion: ellipse equation
  if (env$exp$setup$tracker$calibration == "H3") {
    test <- (vxy$x / radiusx)^2 # 3 point calibration (no y dimension)
  } else {
    test <- (vxy$x / radiusx)^2 + (vxy$y / radiusy)^2
  }
  
  # compute saccades
  # -----------------
  
  indx <- which(test > 1)
  N <- length(indx)
  sac <- NULL
  nsac <- 0
  dur <- 1
  a <- 1
  k <- 1
  while (k < N) {
    if ((indx[k + 1] - indx[k]) == 1) {
      dur <- dur + 1
    } else {
      if (dur >= env$exp$setup$analysis$mindur) {
        nsac <- nsac + 1
        b <- k
        sac <- rbind(sac,c(indx[a], indx[b]))
      }
      a <- k + 1
      dur <- 1
    }
    k <- k + 1
  }
  # check last fixation
  if (dur >= env$exp$setup$analysis$mindur) {
    nsac <- nsac + 1
    sac <- rbind(sac,c(indx[a], indx[N]))
  }
  
  # FIX: check number of saccades
  # -------------------------
  
  if (nsac == 0) {
    sacout <- data.frame(matrix(0, 1, 7))
    names(sacout) <- c('num', 'start', 'stop', 'xs', 'ys', 'xe', 'ye')
    results = list(sac = sacout, radius = radius)
    return(results)
  }
  
  # remove glissades
  # --------------------
  
  # NOTE: be careful, this also removes short blinks within saccade
  
  idx <- 1
  while (idx < nrow(sac)){
    if (sac[idx + 1, 1] - sac[idx, 2] <= env$exp$setup$analysis$postdur) { 
      sac[idx, 2] = sac[idx + 1, 2]	# set saccade
      sac <- sac[-(idx + 1),] 		# remove next row
    } else {
      idx = idx + 1
    }
  }
  # check first fixation
  if (sac[1, 1] <= 5) {
    sac[1, 1] <- 1
  }
  nsac <- nrow(sac)
  
  # saccade position
  sacout <- matrix(NA, nsac, 7)
  for (s in 1:nsac) {
    sacout[s, 1] <- s
    # onset and offset
    sacout[s, 2] <- sac[s, 1]
    sacout[s, 3] <- sac[s, 2]
    # start and end x and y position
    sacout[s, 4] = round(xy$x[sac[s, 1]])
    sacout[s, 5] = round(xy$y[sac[s, 1]])
    sacout[s, 6] = round(xy$x[sac[s, 2]])
    sacout[s, 7] = round(xy$y[sac[s, 2]])
  }
  
  # output
  sacout <- data.frame(sacout)
  names(sacout) <- c('num', 'start', 'stop', 'xs', 'ys', 'xe', 'ye')
  
  # return
  results = list(sac = sacout, radius = radius)
  
  return(results)
  
}
