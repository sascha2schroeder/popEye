
CombineEvents <- function(dat) {
  
  for (trial in 1:length(dat$item)) {
    
    # trial = 1
    
    # fixations
    # ----------
    
    dat$item[[trial]]$fix2 <- data.frame(matrix(NA, nrow(dat$item[[trial]]$fix) , 8))
    colnames(dat$item[[trial]]$fix2) <- c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg")
    
    dat$item[[trial]]$fix2$num <- 1:nrow(dat$item[[trial]]$fix2)
    dat$item[[trial]]$fix2$start <- dat$item[[trial]]$fix$start
    dat$item[[trial]]$fix2$stop <- dat$item[[trial]]$fix$stop
    dat$item[[trial]]$fix2$xs <- dat$item[[trial]]$fix$xs
    dat$item[[trial]]$fix2$ys <- dat$item[[trial]]$fix$ys
    dat$item[[trial]]$fix2$msg <- "FIX"
    
    
    # saccades
    # ---------
    
    dat$item[[trial]]$sac2 <- dat$item[[trial]]$sac
    names = c("sacid", "start", "stop", "xs", "ys", "xe", "ye", "msg")
    dat$item[[trial]]$sac2 <- dat$item[[trial]]$sac2[, match(names, colnames(dat$item[[trial]]$sac2))]
    colnames(dat$item[[trial]]$sac2) <- c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg")
    
    # msg
    # ----
    
    dat$item[[trial]]$msg2 <- data.frame(matrix(NA, nrow(dat$item[[trial]]$msg), 8))
    colnames(dat$item[[trial]]$msg2) <- c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg")
    
    dat$item[[trial]]$msg2$num <- 1:nrow(dat$item[[trial]]$msg)
    dat$item[[trial]]$msg2$start <- dat$item[[trial]]$msg$time
    dat$item[[trial]]$msg2$msg <- dat$item[[trial]]$msg$msg
    
    # set message stop time for sorting
    dat$item[[trial]]$msg2$stop <- 0
    
    # combine
    # --------
    
    dat$item[[trial]]$all <- 
      rbind(dat$item[[trial]]$fix2, dat$item[[trial]]$sac2, dat$item[[trial]]$msg2)
    dat$item[[trial]]$all <- 
      dat$item[[trial]]$all[order(dat$item[[trial]]$all$start, dat$item[[trial]]$all$stop), ]
    dat$item[[trial]]$all$num <- 1:nrow(dat$item[[trial]]$all)
    row.names(dat$item[[trial]]$all) <- NULL
    dat$item[[trial]]$fix2 <- NULL
    dat$item[[trial]]$sac2 <- NULL
    dat$item[[trial]]$msg2 <- NULL
    
    
    # create variables
    # ---------------
    
    dat$item[[trial]]$all$dur <- dat$item[[trial]]$all$stop - dat$item[[trial]]$all$start + 1
    dat$item[[trial]]$all$dur[is.na(dat$item[[trial]]$all$stop) == T] <- NA
    
    # recode negative durations for msg events
    dat$item[[trial]]$all$dur[is.na(dat$item[[trial]]$all$msgnum) == F] <- dat$item[[trial]]$all$dur[is.na(dat$item[[trial]]$all$msgnum) == F] * -1
    
    dat$item[[trial]]$all$type <- 3
    dat$item[[trial]]$all$type[dat$item[[trial]]$all$msg == "FIX"] <- 1
    dat$item[[trial]]$all$type[dat$item[[trial]]$all$msg == "SAC" | dat$item[[trial]]$all$msg == "BLINK"] <- 2
    dat$item[[trial]]$all$typenum <- ave(dat$item[[trial]]$all$num, dat$item[[trial]]$all$type, FUN = rank)
    dat$item[[trial]]$all$fixnum <- NA
    dat$item[[trial]]$all$fixnum[dat$item[[trial]]$all$msg == "FIX"] <- dat$item[[trial]]$all$typenum[dat$item[[trial]]$all$msg == "FIX"]
    dat$item[[trial]]$all$sacnum <- NA
    dat$item[[trial]]$all$sacnum[dat$item[[trial]]$all$msg == "SAC" | dat$item[[trial]]$all$msg == "BLINK"] <- dat$item[[trial]]$all$typenum[dat$item[[trial]]$all$msg == "SAC" | dat$item[[trial]]$all$msg == "BLINK"]
    dat$item[[trial]]$all$msgnum <- NA
    dat$item[[trial]]$all$msgnum[dat$item[[trial]]$all$msg != "FIX" & dat$item[[trial]]$all$msg != "SAC" & dat$item[[trial]]$all$msg != "BLINK"] <- dat$item[[trial]]$all$typenum[dat$item[[trial]]$all$msg != "FIX" & dat$item[[trial]]$all$msg != "SAC" & dat$item[[trial]]$all$msg != "BLINK"]
    
    names <- c("num", "msgnum", "fixnum", "sacnum", "msg", "start", "stop", "dur", "xs", "ys", "xe", "ye")
    dat$item[[trial]]$all <- dat$item[[trial]]$all[names]
    
    
    # delete short last fixation
    # ---------------------------
    
    # if (dat$item[[trial]]$all$msg[nrow(dat$item[[trial]]$all) - 1] == "FIX" & 
    #     (dat$item[[trial]]$all$stop[nrow(dat$item[[trial]]$all) -1] - 
    #     dat$item[[trial]]$all$start[nrow(dat$item[[trial]]$all) -1]) < 50) {
    #   dat$item[[trial]]$all <- dat$item[[trial]]$all[-(nrow(dat$item[[trial]]$all) - 1), ]  
    #   print(dat$item[[trial]]$all)
    # }
    # TODO: here or in DeleteFixations 
    
    # delete message stop time (needed for sorting)
    # ----------------------------------------------
    
    dat$item[[trial]]$all$stop[dat$item[[trial]]$all$stop == 0] <- NA
    
  }
  
  return(dat)
  
}
