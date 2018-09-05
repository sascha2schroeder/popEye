
Preprocessing <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: align function
  
  # message("... processing ...")
  
  # prepare slots for trials
  ret <- rep(list(NA), length(table(dat$msg$trialnum)))
  
  # TODO: maybe separate restructuring function
  
  
  # trial loop
  # -----------
  
  for (trial in 1:length(table(dat$msg$trialnum))) {
  # for (trial in 65:65) {
    # trial <- 63
    
    start <- RetrieveStartStop(dat, trial)$start
    stop <- RetrieveStartStop(dat, trial)$stop

    # if (start == Inf) next # FIX: if trial is empty
    
    tmp <- SelectTrial(dat, start, stop)
    tmp <- TrialTime(tmp) # -> part of SelectTrial() ?

    
    # create trial slot
    # ------------------
    
    meta <- list(trialid = trial, trialnum = max(tmp$msg$trialnum), itemid = max(tmp$msg$itemid), condition = max(tmp$msg$condition), dependency = max(tmp$msg$dependency))
    tmp$msg$trialnum <- NULL # remove trialnum from msg object
    tmp$msg$itemid <- NULL # remove condition from msg object
    tmp$msg$condition <- NULL # remove condition from msg object
    tmp$msg$dependency <- NULL # remove condition from msg object
    
    if (sum(tmp$event$msg == "SFIX") >= 3) { # FIX: skip if there are less than three fixations in trial
      
      if (nrow(tmp$samp) == 0 | mean(is.na(tmp$samp$x)) > .75) { # FIX: if trial is (nearly) empty
        
        xy <- NULL
        vxy <- NULL
        
        # parse events
        # -------------
        
        out <- EventLong(TimestampToEvent(tmp))
        
      } else {
        
        xy <- SmoothData(tmp$samp[, c("time", "x", "y")])
        vxy <- ComputeVelocity(xy, type = 2)
        
        # parse events
        # -------------
        
        if (env$exp$setup$analysis$eyelink == FALSE) {
          out <- EventLong(ComputeEvents(xy, vxy))  
        } else {
          out <- EventLong(TimestampToEvent(tmp))  
        }
        
      }
     
      
      # clean
      # -----------
      
      if (sum(out$msg == "SAC") > 0) { # QUICK FIX: do not clean if no saccade detected
        clean <- Cleaning(out)  
      } else {
        clean <- NA
      }    
      
      
      # align
      # ----------------
      
      # # TODO: adjust to tmplong format
      # if (align == T) {
      #   out$fix <- AlignFixations(out$fix)
      # }
      
    } else {
      
      xy <- NA
      vxy <- NA
      clean <- NA
      
    }
    
    # write out
      ret[[trial]] <- list(meta = meta,
                          msg = tmp$msg,
                          samp = tmp$samp,
                          event = tmp$event,
                          xy = xy,
                          vxy = vxy,
                          parse = clean)
      
  }
  
  
  # check for empty slots
  for (i in length(ret):1) {
    
    if (length(ret[[i]]$parse) == 1) {
      ret[[i]] <- NULL
    }
    
  }
  
  dat$trial <- ret
  dat$msg <- NULL
  dat$samp <- NULL
  dat$event <- NULL
  
  return(dat)
  
}  
