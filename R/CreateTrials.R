
CreateTrials <- function(dat, env = parent.frame(n = 1)) {
  
  # trial loop
  # -----------
  
  if (is.null(env$select.trials) == T) {
    trials <- unique(dat$msg$trialid)
  } else {
    trials <- env$select.trials
  }
  
  if (is.null(env$skip.trials) == F) {
    trials <- trials[(trials %in% env$skip.trials) == F]
  }
  
  # prepare slots for trials
  ret <- rep(list(NA), length(trials))
  
  num <- 0
  for (trial in trials) {

    num <- num + 1
    
    start <- min(dat$msg$time[dat$msg$trialid == trial])
    stop <- max(dat$msg$time[dat$msg$trialid == trial])
    
    tmp <- SelectTrial(dat, start, stop)
    tmp <- TrialTime(tmp) # -> part of SelectTrial() ?
    
    
    # create meta slot
    # ------------------
    
    time <- env$header$trial$time[trial]
    
    if (is.null(env$header$calibration$time) == F) {
      
      sel <- tail(env$header$calibration[env$header$calibration$time < time, ], n = 1)
      
      if(length(sel$method) == 0) {
        
        meta <- list(trialid = max(tmp$msg$trialid),
                     trialnum = max(tmp$msg$trialnum), 
                     itemid = max(tmp$msg$itemid), 
                     condition = max(tmp$msg$condition), 
                     dependency = max(tmp$msg$dependency),
                     start = time,
                     calibration.method = "",
                     calibration.eye = "",
                     calibration.avg = "",
                     calibration.max = "",
                     drift = "",
                     drift.x = "",
                     drift.y = ""
        )    
        
      } else {
        
        meta <- list(trialid = max(tmp$msg$trialid), 
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
        
      }
      
    } else {
     
      meta <- list(trialid = max(tmp$msg$trialid),
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
    
    # FIX: select left eye if tracking was binocular (corresponds to sample data)
    if (env$header$calibration$eye == "LR") {
      tmp$event <- tmp$event[tmp$event$eye == "L", ]
    }
    
    # FIX: skip if there are less than three fixations in trial
    # FIX: exclude trials with negative x and y values?
    count <- 0
    if (sum(tmp$event$msg == "EFIX" & tmp$event$xs > 0 & tmp$event$ys > 0, na.rm = T) > 2) { 
      
    # TODO: this only works for Eyelink -> FIX
    # TODO: define as parameter?
      
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
      # ------
      
      if (sum(out$msg == "SAC") > 0) { # FIX: do not clean if no saccade detected
        clean <- Cleaning(out)
      } else {
        clean <- NA
      }
      
    } else {
      
      count <- count + 1
      xy <- NA
      vxy <- NA
      clean <- NA
      
    }
    
      ret[[num]] <- list(meta = meta,
                           msg = tmp$msg,
                           samp = tmp$samp,
                           event = tmp$event,
                           xy = xy,
                           vxy = vxy,
                           parse = clean)

  }
  
  # check for empty slots and save
  for (i in length(ret):1) {
    
    if (is.na(ret[[i]]$parse) == T) {
      ret[[i]] <- NULL
    } 
    
  }
    
  dat$item <- ret
  env$header$exclusion <- env$header$exclusion + count
  
  dat$msg <- NULL
  dat$samp <- NULL
  dat$event <- NULL
  env$header$trial <- NULL
  env$meta <- NULL
  
  return(dat)
  
}  
