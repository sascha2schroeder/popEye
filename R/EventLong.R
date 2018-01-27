
# NOTE: to be used with output of TimestampToEvent() with eyelink = T

EventLong <- function(out) {
  
  out2 <- out
  
  out2$sac$msg <- "SAC"
  out2$fix$msg <- "FIX"
  
  if (nrow(out2$blink) > 0) {
    out2$blink$msg <- "BLINK"
  }

  # re-assign fixatons with NA to blinks
  for (i in 1:nrow(out2$fix)) {
    if (is.na(out2$fix$xs[i]) == T) {
      out2$blink <- rbind(out2$blink, out2$fix[i, ])
      out2$fix <- out2$fix[-i, ]
    }
  }
  
  # re-assign blink message
  if (nrow(out2$blink) > 0) {
    out2$blink$msg <- "BLINK"
  }
  
  # combine  
  tmplong <- rbind(out2$sac, out2$fix, out2$blink)
  tmplong <- tmplong[order(tmplong$start), ]
  
  
  # screen for empty events
  # ------------------------
  
  tmplong$dur <- tmplong$stop - tmplong$start
  tmplong <- tmplong[tmplong$dur > 0, ]
  tmplong$dur <- NULL
  
  
  # integrate BLINK and SAC
  # ------------------------
  
  # NOTE: assumes that first event is not BLINK
  
  tmplong$del <- 0
  for (i in 1:nrow(tmplong)) {
    if (tmplong$msg[i] == "BLINK") {
      if (i == 1) next # FIX
     tmplong$start[i] <- tmplong$start[i - 1]
     tmplong$stop[i] <- tmplong$stop[i - 1]
     tmplong$xs[i] <- tmplong$xs[i - 1]
     tmplong$ys[i] <- tmplong$ys[i - 1]
     tmplong$xe[i] <- tmplong$xe[i - 1]
     tmplong$ye[i] <- tmplong$ye[i - 1]
     tmplong$del[i - 1] <- 1
    }
  }
  tmplong <- tmplong[tmplong$del == 0, ]
  tmplong$del <- NULL
  tmplong$num <- 1:nrow(tmplong)

  
  # compute blinks
  # ---------------
  
  tmplong$blink.before <- 0
  tmplong$blink.after <- 0
  
  for (i in 1:nrow(tmplong)) {
    if (tmplong$msg[i] == "BLINK") {
      tmplong$blink.after[i - 1] <- 1
      if (is.na(tmplong$msg[i + 1]) == F) {
        tmplong$blink.before[i + 1] <- 1  
      }
    }
  }
  
  # combine 
  tmplong$blink <- 0
  tmplong$blink[tmplong$blink.before == 1 | tmplong$blink.after == 1] <- 1
  
  tmplong$blink.before <- NULL
  tmplong$blink.after <- NULL
  
  # save
  
  return(tmplong) 
  
}

