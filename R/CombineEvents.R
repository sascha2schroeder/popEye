
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
    names = c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg")
    dat$trial[[trial]]$sac2 <- dat$trial[[trial]]$sac2[, match(names, colnames(dat$trial[[trial]]$sac2))]
    
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
    
    # delete message stop time needed for sorting
    dat$trial[[trial]]$all$stop[dat$trial[[trial]]$all$stop == 0] <- NA
    
  }
  
  return(dat)
  
}
