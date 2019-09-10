
CreateTrials <- function(dat, env = parent.frame(n = 1)) {
  
  # prepare slots for trials
  ret <- rep(list(NA), length(table(dat$msg$trialnum)))
  
  
  # trial loop
  # -----------
  
  if (is.null(env$select.trial) == T) {
    trial.arg1 <- 1
    trial.arg2 <- length(table(dat$msg$trialnum))
  } else {
    trial.arg1 <- env$select.trial
    trial.arg2 <- env$select.trial
  }
  
  for (trial in trial.arg1:trial.arg2) {
    
    start <- RetrieveStartStop(dat, trial)$start
    stop <- RetrieveStartStop(dat, trial)$stop

    tmp <- SelectTrial(dat, start, stop)
    tmp <- TrialTime(tmp) # -> part of SelectTrial() ?
    
    
    # create meta slot
    # ------------------
    
    time <- env$header$trial$time[trial]
    
    if (is.null(env$header$calibration$time) == F) {
      
      sel <- tail(env$header$calibration[env$header$calibration$time < time, ], n = 1)
      
      meta <- list(trialid = trial, 
                   trialnum = max(tmp$msg$trialnum), 
                   itemid = max(tmp$msg$itemid), 
                   condition = max(tmp$msg$condition), 
                   dependency = max(tmp$msg$dependency),
                   start = time,
                   calibration.method = sel$method,
                   calibration.eye = sel$eye,
                   calibration.avg = as.numeric(sel$avg),
                   calibration.max = as.numeric(sel$max),
                   drift = env$header$trial$drift[trial],
                   drift.x = as.numeric(as.character(env$header$trial$drift.x[trial])),
                   drift.y = as.numeric(as.character(env$header$trial$drift.y[trial]))
      )    
      
    } else {
     
      meta <- list(trialid = trial, 
                   trialnum = max(tmp$msg$trialnum), 
                   itemid = max(tmp$msg$itemid), 
                   condition = max(tmp$msg$condition), 
                   dependency = max(tmp$msg$dependency),
                   start = time,
                   calibration.method = env$header$calibration$method,
                   calibration.eye = env$header$calibration$eye
                   )
                   
    }
    
    tmp$msg$trialnum <- NULL # remove trialnum from msg object
    tmp$msg$itemid <- NULL # remove condition from msg object
    tmp$msg$condition <- NULL # remove condition from msg object
    tmp$msg$dependency <- NULL # remove condition from msg object
    
    
    # create event slot
    # ------------------
    
    if (env$header$calibration$eye == "LR") {
      tmp$event <- tmp$event[tmp$event$eye == "L", ]
    }
    # FIX: select left eye if tracking was binocular (corresponds to sample data)
    
    
    if (sum(tmp$event$msg == "SFIX") >= 3) { # FIX: skip if there are less than three fixations in trial
    # TODO: this only works for Eyelink -> FIX
      
      if (nrow(tmp$samp) == 0 | mean(is.na(tmp$samp$x)) > .75) { # FIX: if trial is (nearly) empty
        
        xy <- NULL
        vxy <- NULL
        
        # parse events
        # -------------
        
        out <- EventLong(TimestampToEvent(tmp))
        
      } else {
        
        xy <- SmoothData(data.frame(tmp$samp[, c("time", "x", "y")]))
        vxy <- ComputeVelocity(xy, type = 2)
        
        # parse events
        # -------------
        
        if (env$exp$setup$analysis$eyelink == FALSE) {
          
          out <- ComputeEvents(xy, vxy) 
          
        } else {
          
          out <- EventLong(TimestampToEvent(tmp))  
          
        }
        
      }
     
      # clean
      # -----------

      if (sum(out$msg == "SAC") > 0) { # FIX: do not clean if no saccade detected
        clean <- Cleaning(out)
      } else {
        clean <- NA
      }
      
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
  
  # check for empty slots and save
  if (is.null(env$select.trial) == T) {
    
    for (i in length(ret):1) {
      if (length(ret[[i]]$parse) == 1) {
        ret[[i]] <- NULL
      }
    }
    
    dat$trial <- ret
    
  } else {
    
    dat$trial[[1]] <- ret[[env$select.trial]]
    
  }
  
  dat$msg <- NULL
  dat$samp <- NULL
  dat$event <- NULL
  env$header$trial <- NULL
  env$header$calibration <- NULL
  env$meta <- NULL
  
  return(dat)
  
}  
