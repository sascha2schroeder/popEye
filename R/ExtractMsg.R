
ExtractMsg <- function(infile, env = parent.frame(n = 2)) {
  
  # remove non-message events
  dat <- infile[grep("MSG", infile, useBytes = T)]
  
  
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
    itemid <- sapply(strsplit(tmp[ind == env$exp$setup$variable$id], " "), "[[", 5)
    
    # number of practice trials
    env$exp$setup$clean$practice <- env$exp$setup$item$pracnum
    
    # condition
    if (is.na(env$exp$setup$variable$cond) == FALSE) {
      
      if (length(env$exp$setup$variable$cond) == 1) {
        
        tmp <- dat[grep("TRIAL_VAR", dat)]
        ind <- sapply(strsplit(tmp, " "), "[[", 4)
        condition <- sapply(strsplit(tmp[ind == env$exp$setup$variable$cond], " "), "[[", 5)
        
      } else if (length(env$exp$setup$variable$cond) == 2) {
        
        tmp <- dat[grep("TRIAL_VAR", dat)]
        
        ind <- sapply(strsplit(tmp, " "), "[[", 4)
        cond1 <- sapply(strsplit(tmp[ind == env$exp$setup$variable$cond[1]], " "), "[[", 5)
        cond2 <- sapply(strsplit(tmp[ind == env$exp$setup$variable$cond[2]], " "), "[[", 5)
        condition <- paste(cond1, cond2, sep = ":")
        
      }
      
    } else {
      
      condition <- rep(1, length(itemid))
    }
    
    # dependency
    dependency <- as.numeric(rep(0, length(itemid)))
    # NOTE: does not make much sense; store to be parallel with ET
    
    
    # driftcorrect
    # -------------
    
    # NOTE: check whether driftcorrect is present for EB
    
    # extract driftcorrect elements
    tmp <- dat[grep("DRIFTCORRECT", dat)]
    
    # remove repetitions
    tmp <- tmp[-grep("REPEATING", tmp)]
   
    # remove aborted trials
    if (length(grep("ABORTED", tmp)) > 0) {
      tmp <- tmp[-grep("ABORTED", tmp)]
    }
    
    time <- as.numeric(sapply(strsplit(sapply(strsplit(
      tmp, " "), "[[", 1), "\t"), "[[", 2))
    drift <- as.numeric(sapply(strsplit(tmp, " "), "[[", 9))
    x <- sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 12), ","), "[[", 1)
    y <- sapply(strsplit(sapply(strsplit(tmp, " "), "[[", 12), ","), "[[", 2)
    
    env$header$drift <- data.frame(time = time, trialnum = trialnum, drift = drift, x = x, y = y)
      
  }
  
  # ET
  if (env$exp$setup$tracker$software == "ET") {
    
    # extract itemid
    itemtmp <- sapply(strsplit(tmp[grep("TRIALID", tmp)], " "), "[[", 3)
    
    # number of practice trials
    env$exp$setup$clean$practice <-itemtmp[grep(env$exp$setup$item$practice, itemtmp)]
    
    # remove letters from itemid
    tmp <- strsplit(itemtmp, "P|E|I|D")

    # itemid
    itemid <- itemtmp
    
    # condition
    if (is.na(env$exp$setup$stimulus$cond) == FALSE) {
      condition <- sapply(tmp, "[[", 2)
      } else {
      condition <- rep(1, length(itemid))
    }
    
    # dependency
    dependency <- as.numeric(sapply(tmp, "[[", 4)) # save for later use (multiple-screen) texts
  }
  
  # trial slot
  trial <- data.frame(time = time, trialnum = trialnum, itemid = itemid,
                      condition = condition, dependency = dependency,
                      stringsAsFactors = FALSE)
  env$header$trial <- trial
  
  
  # create message frame
  # ---------------------
  
  # FIX: if start message is empty
  if (env$exp$setup$message$start == "" | env$exp$setup$message$start == "DRAW_LIST") {
    env$exp$setup$message$start <- "DRAW_LIST"
    dat <- gsub("!V DRAW_LIST", "DRAW_LIST", dat)
  }
  
  start <- dat[grep(paste("\\b", env$exp$setup$message$start, "\\b", sep = ""), dat)]
  stop <- dat[grep(paste("\\b", env$exp$setup$message$stop, "\\b", sep = ""), dat)]
  
  if (env$exp$setup$type == "boundary") {
    boundary <- dat[grep(paste("\\b", env$exp$setup$message$boundary, "\\b", sep = ""), dat)]
    target <- dat[grep(paste("\\b", env$exp$setup$message$target, "\\b", sep = ""), dat)]
    tmp <- c(start, boundary, target, stop)
  } else if (env$exp$setup$type == "fast") {
    boundary <- dat[grep(paste("\\b", env$exp$setup$message$boundary, "\\b", sep = ""), dat)]
    prime <- dat[grep(paste("\\b", env$exp$setup$message$prime, "\\b", sep = ""), dat)]
    target <- dat[grep(paste("\\b", env$exp$setup$message$target, "\\b", sep = ""), dat)]
    tmp <- c(start, boundary, prime, target, stop)
  } else {
    tmp <- c(start, stop)
  }
  
  # exclude variable statements for EB
  if (env$exp$setup$tracker$software == "EB" & length(grep("!V", tmp))) {
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
  
  # sort by timestamp
  msg <- msg[order(msg$time), ]
  
  return(msg)
  
}
