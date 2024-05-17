
ComputeEvents <- function(xy, vxy, env = parent.frame(n = 1)) {
 
  # computes saccades, fixations, and blinks
  # -----------------------------------------
  
  # SAC
  sac <- ComputeSaccades(xy, vxy, env$meta$calibration.method)$sac
  
  # CHECK: saccade positions correct?
  
  # FIX: check number of saccades
  # ------------------------------
  if (sum(sac$start > 0) == 0) {
    fix = data.frame(matrix(0, 1, 9))
    colnames(fix) = c("num", "start", "stop", "xs", "ys", "xe", "ye", 
                      "xdrift", "ydrift")  
  } else {
    # FIX
    fix <- ComputeFixations(xy, sac)
  }
  
  # BLINK
  blink <- fix[is.na(fix$xs) == T, ]
  
  
  # integrate events
  # -----------------
  
  sac$msg <- "SAC"
  if (nrow(blink) > 0) {
    blink$msg <- "BLINK"
  }
  fix$msg <- "FIX"
  
  out <- rbind(sac, fix, blink)
  out <- out[order(out$start), ]
  
  out$del <- 0
  for (i in 1:nrow(out)) {
    if (out$msg[i] == "BLINK") {
      out$del[i - 1] <- 1
    }
  }
  
  out2 <- out[out$del == 0, ]
  out2$before <- 0
  out2$after <- 0
  
  for (i in 2:nrow(out2)) {
    if (out2$msg[i] == "BLINK") {
      out2$before[i - 1] <- 1
    }
  }
  
  for (i in 1:(nrow(out2) - 1)) {
    if (out2$msg[i] == "BLINK") {
      out2$after[i + 1] <- 1
    }
  }
 
  out2$del <- 0
  for (i in 2:nrow(out2)) {
    if (out2$before[i] == 1 & out2$after[i] == 1) {
      out2$stop[i - 1] <- out2$stop[i + 1]
      out2$xs[i - 1] <- NA
      out2$ys[i - 1] <- NA
      out2$del[i] <- 1
      out2$del[i + 1] <- 1
    }
  }
  out2 <- out2[out2$del == 0, ]
  
  for (i in 2:nrow(out2)) {
    if (out2$before[i] == 1) {
      out2$start[i + 1] <- out2$start[i]
      out2$xs[i + 1] <- out2$xs[i]
      out2$ys[i + 1] <- out2$ys[i]
      out2$del[i] <- 1
    }
  }
  
  for (i in 1:(nrow(out2) - 1)) {
    if (out2$after[i] == 1) {
      out2$stop[i - 1] <- out2$stop[i]
      out2$xe[i - 1] <- out2$xe[i]
      out2$ye[i - 1] <- out2$ye[i]
      out2$del[i] <- 1
    }
  }
 
  out3 <- out2[out2$del == 0, ]
  out3$num <- 1:nrow(out3)
  out3$del <- NULL
  out3$before <- NULL
  out3$after <- NULL
  
  out3$blink <- 0
  for (i in 2:(nrow(out3) - 1)) {
    if (out3$msg[i] == "BLINK") {
      out3$blink[i - 1] <- 1
      out3$blink[i + 1] <- 1
    }
  }

  return(out3)
  
}
