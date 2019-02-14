
TimestampToEvent <- function(dat) {
  
  # TODO: does not work for trials starting with SFIX (?)

    
  # initialize output matrices
  # ---------------------------
  
  fix <- data.frame(matrix(NA, 1, 7))
  colnames(fix) <- c("num", "start", "stop", "xs", "ys", "xe", "ye") 
  fixnum <- 0
  
  sac <- data.frame(matrix(NA, 1, 7))
  colnames(sac) <- c("num", "start", "stop", "xs", "ys", "xe", "ye") 
  sacnum <- 0
  
  blink <- data.frame(matrix(NA, 1, 7))
  colnames(blink) <- c("num", "start", "stop", "xs", "ys", "xe", "ye") 
  blinknum <- 0
  
  
  # evaluate first message
  # ------------------------
  
  if (dat$event$msg[1] == "EFIX") {
    fixnum <- 1
    fix$num[fixnum] <- 1
    fix$start[fixnum] <- 1
  } else if (dat$event$msg[1] == "ESACC") {
    sacnum <- 1
    sac$num[sacnum] <- 1
    sac$start[sacnum] <- 1
  } else if (dat$event$msg[1] == "SFIX") {
    fixnum <- fixnum
  } else if (dat$event$msg[1] == "EBLINK") {
    blinknum <- 1
    blink$num[blinknum] <- 1
    blink$start[blinknum] <- 1
  } else {
    print(paste("First element", dat$event$msg[1], "not defined yet.", sep = " "))
  }
  
  
  # timestamp loop
  # ---------------
  for (i in 1:nrow(dat$event)){
    # i = 1
    
    switch(dat$event$msg[i],
           "SFIX" = {
             fixnum <- fixnum + 1
             fix <- fix[1:fixnum, ]
             fix$num[fixnum] <- fixnum
             fix$start[fixnum] <- dat$event$time[i]
           },
           "EFIX" = {
             fix$stop[fixnum] <- dat$event$time[i]
             fix$xs[fixnum] <- dat$event$xs[i]
             fix$ys[fixnum] <- dat$event$ys[i]
           },
           "SSACC" = {
             sacnum <- sacnum + 1
             sac <- sac[1:sacnum, ]
             sac$num[sacnum] <- sacnum
             sac$start[sacnum] <- dat$event$time[i]
           },
           "ESACC" = {
             sac$stop[sacnum] <- dat$event$time[i]
             sac$xs[sacnum] <- dat$event$xs[i]
             sac$ys[sacnum] <- dat$event$ys[i]
             sac$xe[sacnum] <- dat$event$xe[i]
             sac$ye[sacnum] <- dat$event$ye[i]
           },
           "SBLINK" = {
             blinknum <- blinknum + 1
             blink <- blink[1:blinknum, ]
             blink$num[blinknum] <- blinknum
             blink$start[blinknum] <- dat$event$time[i]
           },
           "EBLINK" = {
             blink$stop[blinknum] <- dat$event$time[i]
           }
    )
    
    # print(i)
  }
  
  
  # clean and write out
  # --------------------
  
  fix <- fix[is.na(fix$num) == F | is.na(fix$xs) == F, ]
  fix <- fix[is.na(fix$stop) == F, ]
  # fix$stop[nrow(fix)][is.na(fix$stop[nrow(fix)]) == T] <- (max(dat$event$time) - start + 1)
  row.names(fix) <- NULL
  
  sac <- sac[is.na(sac$num) == F | is.na(sac$xs) == F | is.na(sac$stop) == F, ]
  sac <- sac[is.na(sac$stop) == F, ]
  # sac$stop[nrow(sac)][is.na(sac$stop[nrow(sac)]) == T] <- (max(dat$event$time) - start + 1)
  row.names(sac) <- NULL
  
  blink <- blink[is.na(blink$num) == F, ]
  blink <- blink[is.na(blink$stop) == F, ]
  # blink$stop[nrow(blink)][is.na(blink$stop[nrow(blink)]) == T] <- (max(dat$event$time) - start + 1)
  row.names(blink) <- NULL
  
  out <- list(sac = sac, fix = fix, blink = blink)

  return(out)
  
}
