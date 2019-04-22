
CombineEvents <- function(dat) {
  
  for (trial in 1:length(dat$trial)) {
    
    # trial = 1
    
    # fixations
    # ----------
    
    dat$trial[[trial]]$fix2 <- data.frame(matrix(NA, nrow(dat$trial[[trial]]$fix) , 8))
    colnames(dat$trial[[trial]]$fix2) <- c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg")
    
    dat$trial[[trial]]$fix2$num <- 1:nrow(dat$trial[[trial]]$fix2)
    dat$trial[[trial]]$fix2$start <- dat$trial[[trial]]$fix$start
    dat$trial[[trial]]$fix2$stop <- dat$trial[[trial]]$fix$stop
    dat$trial[[trial]]$fix2$xs <- dat$trial[[trial]]$fix$xs
    dat$trial[[trial]]$fix2$ys <- dat$trial[[trial]]$fix$ys
    dat$trial[[trial]]$fix2$msg <- "FIX"
    
    
    # saccades
    # ---------
    
    dat$trial[[trial]]$sac2 <- dat$trial[[trial]]$sac
    names = c("sacid", "start", "stop", "xs", "ys", "xe", "ye", "msg")
    dat$trial[[trial]]$sac2 <- dat$trial[[trial]]$sac2[, match(names, colnames(dat$trial[[trial]]$sac2))]
    colnames(dat$trial[[trial]]$sac2) <- c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg")
    
    # msg
    # ----
    
    dat$trial[[trial]]$msg2 <- data.frame(matrix(NA, nrow(dat$trial[[trial]]$msg), 8))
    colnames(dat$trial[[trial]]$msg2) <- c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg")
    
    dat$trial[[trial]]$msg2$num <- 1:nrow(dat$trial[[trial]]$msg)
    dat$trial[[trial]]$msg2$start <- dat$trial[[trial]]$msg$time
    dat$trial[[trial]]$msg2$msg <- dat$trial[[trial]]$msg$msg
    
    # set message stop time for sorting
    dat$trial[[trial]]$msg2$stop <- 0
    
    # combine
    # --------
    
    dat$trial[[trial]]$all <- 
      rbind(dat$trial[[trial]]$fix2, dat$trial[[trial]]$sac2, dat$trial[[trial]]$msg2)
    dat$trial[[trial]]$all <- 
      dat$trial[[trial]]$all[order(dat$trial[[trial]]$all$start, dat$trial[[trial]]$all$stop), ]
    dat$trial[[trial]]$all$num <- 1:nrow(dat$trial[[trial]]$all)
    row.names(dat$trial[[trial]]$all) <- NULL
    dat$trial[[trial]]$fix2 <- NULL
    dat$trial[[trial]]$sac2 <- NULL
    dat$trial[[trial]]$msg2 <- NULL
    
    
    # create variables
    # ---------------
    
    dat$trial[[trial]]$all$dur <- dat$trial[[trial]]$all$stop - dat$trial[[trial]]$all$start + 1
    dat$trial[[trial]]$all$dur[is.na(dat$trial[[trial]]$all$stop) == T] <- NA
    
    dat$trial[[trial]]$all$type <- 3
    dat$trial[[trial]]$all$type[dat$trial[[trial]]$all$msg == "FIX"] <- 1
    dat$trial[[trial]]$all$type[dat$trial[[trial]]$all$msg == "SAC" | dat$trial[[trial]]$all$msg == "BLINK"] <- 2
    dat$trial[[trial]]$all$typenum <- ave(dat$trial[[trial]]$all$num, dat$trial[[trial]]$all$type, FUN = rank)
    dat$trial[[trial]]$all$fixnum <- NA
    dat$trial[[trial]]$all$fixnum[dat$trial[[trial]]$all$msg == "FIX"] <- dat$trial[[trial]]$all$typenum[dat$trial[[trial]]$all$msg == "FIX"]
    dat$trial[[trial]]$all$sacnum <- NA
    dat$trial[[trial]]$all$sacnum[dat$trial[[trial]]$all$msg == "SAC" | dat$trial[[trial]]$all$msg == "BLINK"] <- dat$trial[[trial]]$all$typenum[dat$trial[[trial]]$all$msg == "SAC" | dat$trial[[trial]]$all$msg == "BLINK"]
    dat$trial[[trial]]$all$msgnum <- NA
    dat$trial[[trial]]$all$msgnum[dat$trial[[trial]]$all$msg != "FIX" & dat$trial[[trial]]$all$msg != "SAC" & dat$trial[[trial]]$all$msg != "BLINK"] <- dat$trial[[trial]]$all$typenum[dat$trial[[trial]]$all$msg != "FIX" & dat$trial[[trial]]$all$msg != "SAC" & dat$trial[[trial]]$all$msg != "BLINK"]
    
    names <- c("num", "msgnum", "fixnum", "sacnum", "msg", "start", "stop", "dur", "xs", "ys", "xe", "ye")
    dat$trial[[trial]]$all <- dat$trial[[trial]]$all[names]
    
    
    # delete short last fixation
    # ---------------------------
    
    # if (dat$trial[[trial]]$all$msg[nrow(dat$trial[[trial]]$all) - 1] == "FIX" & 
    #     (dat$trial[[trial]]$all$stop[nrow(dat$trial[[trial]]$all) -1] - 
    #     dat$trial[[trial]]$all$start[nrow(dat$trial[[trial]]$all) -1]) < 50) {
    #   dat$trial[[trial]]$all <- dat$trial[[trial]]$all[-(nrow(dat$trial[[trial]]$all) - 1), ]  
    #   print(dat$trial[[trial]]$all)
    # }
    # TODO: here or in DeleteFixations 
    
    # delete message stop time (needed for sorting)
    # ----------------------------------------------
    
    dat$trial[[trial]]$all$stop[dat$trial[[trial]]$all$stop == 0] <- NA
    
  }
  
  return(dat)
  
}
