
ComputeSaccades <- function(xy, vxy, calibration.method, env = parent.frame(n = 3)) {
  
  # NOTE: saccade "amplitude" in pixels, not degrees at present
  
  
  # determine tresholds
  # ---------------------
  
  radius <- ComputeRadius(vxy)
  
  # compute test criterion: ellipse equation
  # TODO: this should not work as the environment points to the experiment level
  if (calibration.method == "H3") {
    
    test <- (vxy$x / radius$x)^2 # 3 point calibration (no y dimension)
    
  } else {
    
    test <- (vxy$x / radius$x)^2 + (vxy$y / radius$y)^2
    
  }
 
  # TODO: use exact (variable) sampling rate?
  
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
      #dur <- dur + 1
      dur <- dur + (1000 / env$exp$setup$tracker$samp)
    } else {
      if (dur >= env$exp$setup$analysis$mindur) {
        nsac <- nsac + 1
        b <- k
        sac <- rbind(sac,c(xy$time[indx[a]], xy$time[indx[b]]))
      }
      a <- k + 1
      dur <- 1
    }
    k <- k + 1
  }
  # check last fixation
  if (dur >= env$exp$setup$analysis$mindur) {
    nsac <- nsac + 1
    sac <- rbind(sac,c(xy$time[indx[a]], xy$time[indx[N]]))
  }
  
  
  # FIX: check number of saccades
  # ------------------------------
  
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
    
    # FIX: if saccade starts with first sample 
    if(length(xy$x[xy$time == sac[s, 1]]) == 0) {
      sacout[s, 4] = round(xy$x[1])
      sacout[s, 5] = round(xy$y[1])
    } else {
      sacout[s, 4] = round(xy$x[xy$time == sac[s, 1]])
      sacout[s, 5] = round(xy$y[xy$time == sac[s, 1]])
    }
    
    sacout[s, 6] = round(xy$x[xy$time == sac[s, 2]])
    sacout[s, 7] = round(xy$y[xy$time == sac[s, 2]])
  }
  
  # output
  sacout <- data.frame(sacout)
  names(sacout) <- c('num', 'start', 'stop', 'xs', 'ys', 'xe', 'ye')
  
  # return
  results = list(sac = sacout, radius = radius)
  
  return(results)
  
}
