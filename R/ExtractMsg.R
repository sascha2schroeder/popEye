
ExtractMsg <- function(dat, env = parent.frame(n = 3)){
  
  # remove non-message events
  dat <- dat[grep("MSG", dat, useBytes = T)]
  
  
  # extract useful information
  # ---------------------------
  
  # TODO: date (?)
  
  # display coordinates
  # env$exp$setup$display$resolutionX <-
  #   as.numeric(sapply(strsplit(dat[grep("DISPLAY_COORDS", dat)], " "), "[[", 5)) + 1
  # env$exp$setup$display$resolutionY <-
  #   as.numeric(sapply(strsplit(dat[grep("DISPLAY_COORDS", dat)], " "), "[[", 6)) + 1
  
  env$exp$setup$display$resolutionX <-
    as.numeric(sapply(strsplit(dat[grep("GAZE_COORDS", dat)], " "), "[[", 5)[1]) + 1
  env$exp$setup$display$resolutionY <-
    as.numeric(sapply(strsplit(dat[grep("GAZE_COORDS", dat)], " "), "[[", 6)[1]) + 1
  
  # TODO: monitor refresh rate (?)
  
  # sample rate
  env$exp$setup$tracker$samp <-
    as.numeric(sapply(strsplit(dat[grep("RECORD", dat)][1], " "), "[[", 5))
  
  # calibration method
  env$exp$setup$tracker$calibration <-
    sapply(strsplit(dat[grep("CALIBRATION", dat)][1], " "), "[[", 4)
  
  # TODO: what if calibration method changes during experiment?
  
  # calibration accuracy
  tmp <- dat[grep("VALIDATION", dat)]
  
  # check for aborted calibrations
  if (length(grep("ABORTED", tmp)) > 0) {
    tmp <- tmp[-grep("ABORTED", tmp)]  
  }
  tmp <- gsub("  ", " ", tmp)
  env$header$calibration <- as.numeric(sapply(strsplit(tmp, " "), "[[", 9))
  
  # TODO: store as vector or mean?
  
  # # driftcorrect
  # time <- as.numeric(sapply(strsplit(sapply(strsplit(
  #   dat[grep("DRIFTCORRECT", dat)], " "), "[[", 1)), "\t"), "[[", 2)
  # drift <- as.numeric(sapply(strsplit(dat[grep("DRIFTCORRECT", dat)], " "), "[[", 9))
  # DriftCorrect <- data.frame(time = time, drift = drift)
  # 
  # # TODO: not for ET  
  # # TODO: store on trial level
  
  
  # extract trials
  # ---------------
  
  tmp <- sapply(strsplit(dat[grep("TRIALID", dat)], "\t"), "[[", 2)
  time <- as.numeric(sapply(strsplit(tmp[grep("TRIALID", tmp)], " "), "[[", 1))
  trialnum <- 1:length(time)
  
  
  # EB
  if (env$exp$setup$tracker$software == "EB") {
    
    # itemid
    tmp <- dat[grep("TRIAL_VAR", dat)]
    ind <- sapply(strsplit(tmp, " "), "[[", 4)
    itemid <- as.numeric(sapply(strsplit(tmp[ind == env$exp$setup$message$itemid], " "), "[[", 5))
    
    # number of practice trials
    env$exp$setup$clean$practice <- max(as.numeric(unlist(dimnames(table(table(itemid)))))) - 1
    
    # condition
    if (is.na(env$exp$setup$message$condition) == FALSE) {
      tmp <- dat[grep("TRIAL_VAR", dat)]
      ind <- sapply(strsplit(tmp, " "), "[[", 4)
      condition <- sapply(strsplit(tmp[ind == env$exp$setup$message$condition], " "), "[[", 5)
    } else {
      condition <- rep(1, length(itemid))
    }
    
    # dependency
    dependency <- as.numeric(rep(0, length(itemid)))
    # NOTE: does not make much sense; store to be parallel with ET
  }
  
  # ET
  if (env$exp$setup$tracker$software == "ET") {
    
    # extract itemid
    itemtmp <- sapply(strsplit(tmp[grep("TRIALID", tmp)], " "), "[[", 3)
    tmp <- strsplit(itemtmp, "P|E|I|D")
    
    # itemid
    itemid <- as.numeric(sapply(tmp, "[[", 3))
    
    # number of practice trials
    env$exp$setup$clean$practice <- length(itemtmp[grep(env$exp$setup$item$practice, itemtmp)])
   
    # condition
    condition <- sapply(tmp, "[[", 2)
    
    # dependency
    dependency <- as.numeric(sapply(tmp, "[[", 4)) # save for later use (multiple-screen) texts
  }
  
  # trial slot
  trial <- data.frame(time = time, trialnum = trialnum, itemid = itemid,
                      condition = condition, dependency = dependency,
                      stringsAsFactors = FALSE)
  
  # NOTE: save as output slot?
  
  
  # create message frame
  # ---------------------
  
  # select message stamps
  tmp <- dat[grep(paste(unlist(env$exp$setup$message), collapse = "|"), dat, useBytes = T)]
  
  # exclude variable statements for EB
  if (env$exp$setup$tracker$software == "EB") {
    tmp <- tmp[-grep("!V", tmp)]  
  } 
  
  # substract delay from timestamp
  if (env$exp$setup$tracker$software == "EB") {
    time <- as.numeric(sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 1), "\t"), "[[", 2)) -
      as.numeric(sapply(strsplit(tmp, " "), "[[", 2))
  } else if (env$exp$setup$tracker$software == "ET") {
    time <- as.numeric(sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 1), "\t"), "[[", 2))
  } 
  
  # extract messages
  if (env$exp$setup$tracker$software == "EB") {
    msg <- sapply(strsplit(tmp, " "), "[[", 3)  
  } else if (env$exp$setup$tracker$software == "ET") {
    msg <- sapply(strsplit(tmp, " "), "[[", 2)  
  }
  
  # save as data frame
  msg <- data.frame(time = time, msg = msg, stringsAsFactors = F)
  
  # combine with trial-level variables
  msg$trialnum <- NA
  msg$itemid <- NA
  msg$condition <- NA
  msg$dependency <- NA
  for (i in 1:nrow(trial)){
    msg$trialnum[msg$time >= trial$time[i]] <- trial$trialnum[i]
    msg$itemid[msg$time >= trial$time[i]] <- trial$itemid[i]
    msg$condition[msg$time >= trial$time[i]] <- trial$condition[i]
    msg$dependency[msg$time >= trial$time[i]] <- trial$dependency[i]
  }
  
  # write out
  names=c("trialnum", "itemid" , "condition", "dependency", "time", "msg")
  msg <- msg[, match(names, colnames(msg))]
  
  # exclude conditional msg (EB)
  if (length(grep("conditional", msg$msg)) > 0) {
    msg <- msg[-grep("conditional", msg$msg), ]
  }

  # rename display change events (ET)  
  msg$ind <- paste(msg$itemid, msg$msg, sep = ":") 
  msg$msg[msg$msg == "DISPLAY" & duplicated(msg$ind) == FALSE] = env$exp$setup$message$boundary
  msg$msg[msg$msg == "DISPLAY" & duplicated(msg$ind) == TRUE] = env$exp$setup$message$target
  msg$ind <- NULL
  
  return(msg)
  
  
  # # stimulus file
  # # --------------
  #  
  # tmp <- dat[grep("REGION", dat, useBytes = T)]
  # tmp <- gsub("  ", " ", tmp)
  # 
  # time <- as.numeric(sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 1), "\t"), "[[", 2))
  # pos <- as.numeric(sapply(strsplit(tmp, " "), "[[", 4)) + 1
  # letter <- sapply(strsplit(tmp, " "), "[[", 6) 
  # letter[as.numeric(factor(letter)) == 1] <- " "
  # xs <- as.numeric(sapply(strsplit(tmp, " "), "[[", 7))
  # ys <- as.numeric(sapply(strsplit(tmp, " "), "[[", 8))
  # xe <- as.numeric(sapply(strsplit(tmp, " "), "[[", 9)) 
  # ye <- as.numeric(sapply(strsplit(tmp, " "), "[[", 10))
  # 
  # stim <- data.frame(time = time, pos = pos, letter = letter, 
  #                    xs = xs, ys = ys, xe = xe, ye = ye, stringsAsFactors = F)
  # 
  # # add trial-level variables to msg frame
  # stim$trialnum <- NA
  # stim$itemid <- NA
  # 
  # for (i in 1:nrow(trial)){
  #   stim$trialnum[stim$time >= trial$time[i]] <- trial$trialnum[i]
  #   stim$itemid[stim$time >= trial$time[i]] <- trial$itemid[i]
  # }
  # 
  # # write out
  # names=c("trialnum", "itemid" , "time", "pos", "letter", "xs", "ys", "xe", "ye")
  # stim <- stim[, match(names, colnames(stim))]
  # write.table(stim, "stim.dat", sep = "\t", row.names = F) 
  # 
  # 
  # # sentence file 
  # # ---------------
  #
  # stimtab <- unlist(dimnames(table(stim$trialnum)))
  # sent <- stim[duplicated(stim$trialnum) == F, ]
  # names <- c("trialnum", "itemid")
  # sent <- sent[names]
  # sent$sentence <- NA
  # for (trial in 1:length(table(stim$trialnum))) {
  #   # trial <- 2
  #   sent$sentence[trial] <- paste(stim$letter[stim$trialnum == stimtab[trial]], collapse = "")
  # }
  # 
  # write.table(sent, "sent.dat", sep = "\t", row.names = F) 
  
}
