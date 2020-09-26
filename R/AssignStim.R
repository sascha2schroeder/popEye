
AssignStim <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 2
  
  # data
  fix <- dat$item[[trial]]$fix
  stimmat <- dat$item[[trial]]$meta$stimmat
  
  if (env$exp$setup$font$right == T) {
    fix$xs <- env$exp$setup$display$resolutionX - fix$xs
  }
  
  
  # drift correct 
  # --------------
  
  if (is.null(dat$item[[trial]]$meta$drift) == T) {
    dat$item[[trial]]$meta$drift <- NA
  }
  
  # x axis
  if (env$exp$setup$assign$driftX == T) {
    
    if (is.na(dat$item[[trial]]$meta$drift) == F) {
      fix$xn <- fix$xs - dat$item[[trial]]$meta$drift.x
    } else {
      fix$xn <- fix$xs 
    }
    
  } else {
    fix$xn <- fix$xs 
  }
  
  # y axis
  if (env$exp$setup$assign$driftY == T) {
    
    if (is.na(dat$item[[trial]]$meta$drift) == F) {
      fix$yn <- fix$ys - dat$item[[trial]]$meta$drift.y + env$exp$setup$font$height / 2
    } else {
      fix$yn <- fix$ys
    }
    
  } else {
    fix$yn <- fix$ys
  }
  
  
  # check outlier
  # --------------
  
  if (env$exp$setup$assign$outlier == T) {
    fix <- CheckOutlier(fix, stimmat, env$exp$setup$assign$outlierDist)
  } else {
    fix$type <- "in"
  }
  
  if (mean(fix$type == "in") < .1) {
    dat$item[[trial]]$fix <- NULL
    return(dat)
  }
  
  
  # move fixations
  # ---------------
  
  if (env$exp$setup$assign$moveMethod == "hit") {
    
    if (env$exp$setup$assign$moveY == T) {
      fix <- MoveFixationsY(fix, stimmat)
    } 
    
    if (env$exp$setup$assign$moveX == T) {
      fix <- MoveFixationsX(fix, stimmat)
    } 
    
  }
  
  if (env$exp$setup$assign$moveMethod == "area") {
    
    if (env$exp$setup$assign$moveY == T) {
      moveY <- TRUE
    } else {
      moveY <- FALSE
    }
    
    if (env$exp$setup$assign$moveX == T) {
      moveX <- TRUE
    } else {
      moveX <- FALSE
    }
    
    fix <- MoveFixations(fix, stimmat, x.adj=moveX, y.adj=moveY)
    
  }

  
  # line assignment 
  # ----------------
  
  # attach method
  if (env$exp$setup$assign$lineMethod == "attach") {
    
    fix$type <- "in"
    fix$line <- NA
    fix$linerun <- NA
    fix$run <- NA
    
    for (i in 1:nrow(fix)) {
      # i <- 2
      
      out <- abs(fix$yn[i] - stimmat$ym)
      
      if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$assign$outlierY) {
        fix$type[i] <- "out"
        fix$line[i] <- NA
      } else {
        fix$line[i] <- stimmat$line[which.min(out)]
      }
      
    }
    
  }
  
  # chain method
  if (env$exp$setup$assign$lineMethod == "chain") {
    
    # compute distance
    fix$distx <- NA
    fix$distx[2:length(fix$distx)] <- diff(fix$xn)
    fix$disty <- NA
    fix$disty[2:length(fix$disty)] <- diff(fix$yn)
    
    # compute mean y position of lines
    linem <- tapply(stimmat$ym, stimmat$line, mean)
    
    # initialize variables
    fix$type <- "in"
    fix$run <- NA
    fix$linerun <- NA
    fix$linerun[1] <- 1
    fix$line <- NA
    
    mem <- NULL
    start <- 1
    stop <- nrow(linem)
    
    # segment into runs
    for (i in 2:nrow(fix)) {
      
      # determine run break
      if (abs(fix$disty[i]) >= env$exp$setup$font$height * env$exp$setup$assign$lineY | 
          abs(fix$distx[i]) >= env$exp$setup$font$height * env$exp$setup$assign$lineX) {
        
        # assign previous run to line
        mean.y <- mean(fix$yn[fix$linerun == fix$linerun[i - 1]], na.rm = T)
        
        if (mean.y > (max(stimmat$ye) + env$exp$setup$font$height * env$exp$setup$assign$outlierY) | 
            mean.y < (min(stimmat$ys) - env$exp$setup$font$height * env$exp$setup$assign$outlierY)) {
          
          fix$type[fix$linerun == fix$linerun[i - 1]] <- "out"
          
        } else {
          
          out <- NULL
          
          for (j in start:stop) {
            out[j] <- (mean.y - linem[j])^2
          }
          
          fix$line[fix$linerun == fix$linerun[i - 1]] <- which.min(out)
          
        }
        
                fix$linerun[i] <- fix$linerun[i - 1] + 1
        
      } else {
        
        fix$linerun[i] <- fix$linerun[i - 1]
        
      }
      
    }
    
    # assign last run
    mean.y <- mean(fix$yn[fix$linerun == fix$linerun[nrow(fix)]], na.rm = T)
    
    if (mean.y > (max(stimmat$ye) + env$exp$setup$font$height * env$exp$setup$assign$outlierY) | 
        mean.y < (min(stimmat$ys) - env$exp$setup$font$height * env$exp$setup$assign$outlierY)) {
      
      fix$type[fix$linerun == fix$linerun[nrow(fix)]] <- "out"
      
    } else {
      
      out <- NULL
      
      for (j in 1:nrow(linem)) {
        out[j] <- (mean.y - linem[j])^2
      }
      
      fix$line[fix$linerun == fix$linerun[nrow(fix)]] <- which.min(out)
      
    }
    
  }
  
  # regress method
  if (env$exp$setup$assign$lineMethod == "regress") {
   
    fixation_XY <- fix[, c("xn", "yn")]
    line_Y <- tapply(stimmat$ym, stimmat$line, max)
    fix$line <- Regress(fixation_XY, line_Y)
    fix$run <- NA
    fix$linerun <- NA
    
  }
  
  # merge method
  if (env$exp$setup$assign$lineMethod == "merge") {
    
    fix <- BuildSequences(fix)
    fix <- Phase1(fix, stimmat)
    fix <- Phase2(fix, stimmat)
    fix <- Phase3(fix, stimmat)
    fix <- Phase4(fix, stimmat)
    fix <- Phase5(fix, stimmat)
    fix <- AssignLine(fix, stimmat)
    
  }
  
  # method interactive
  if (env$exp$setup$assign$lineMethod == "interactive") {
    
    fix <- BuildSequences(fix)
    fix <- SelectLine(fix, stimmat)
    fix <- LineInteractive(fix, stimmat)
    
  }
  
  
  # map letter and IA
  # ------------------
  
  fix$subid <- stimmat$subid[1]
  fix$trialid <- stimmat$trialid[1]
  fix$trialnum <- stimmat$trialnum[1]
  fix$itemid <- stimmat$itemid[1]
  fix$cond <- stimmat$cond[1]
  
  fix$letternum <- NA
  fix$letter <- NA
  fix$wordnum <- NA
  fix$word <- NA
  fix$sentnum <- NA
  fix$sent <- NA
  fix$sent.nwords <- NA
  fix$ianum <- NA
  fix$ia <- NA
  
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    fix$target <- NA
  }
  
  fix$line.let <- NA
  fix$word.land <- NA
  fix$ia.land <- NA
  fix$line.word <- NA
  fix$sent.word <- NA
  
  fix$trial.nwords <- NA
  fix$trial <- NA
  
  for (i in 1:nrow(fix)) {
    # i <- 1
    
    if (fix$type[i] == "in" & fix$line[i] > 0 & is.na(fix$line[i]) == F) {
      
      out <- abs(fix$xn[i] - stimmat$xm[stimmat$line == fix$line[i]])
      
      fix$letternum[i] <- stimmat$letternum[stimmat$line == fix$line[i]][which.min(out)]
      fix$letter[i] <- stimmat$letter[stimmat$line == fix$line[i]][which.min(out)]
      fix$wordnum[i] <- stimmat$wordnum[stimmat$line == fix$line[i]][which.min(out)]
      fix$word[i] <- stimmat$word[stimmat$line == fix$line[i]][which.min(out)]
      fix$sentnum[i] <- stimmat$sentnum[stimmat$line == fix$line[i]][which.min(out)]
      fix$sent[i] <- stimmat$sent[stimmat$line == fix$line[i]][which.min(out)]
      fix$sent.nwords[i] <- stimmat$sent.nwords[stimmat$line == fix$line[i]][which.min(out)]
      fix$ianum[i] <- stimmat$ianum[stimmat$line == fix$line[i]][which.min(out)]
      fix$ia[i] <- stimmat$ia[stimmat$line == fix$line[i]][which.min(out)]
      
      if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
        fix$target[i] <- stimmat$target[stimmat$line == fix$line[i]][which.min(out)]
      }
      
      fix$line.let[i] <- stimmat$letline[stimmat$line == fix$line[i]][which.min(out)]
      fix$word.land[i] <- stimmat$letword[stimmat$line == fix$line[i]][which.min(out)]
      fix$ia.land[i] <- stimmat$letia[stimmat$line == fix$line[i]][which.min(out)]
      fix$line.word[i] <- stimmat$wordline[stimmat$line == fix$line[i]][which.min(out)]
      fix$sent.word[i] <- stimmat$wordsent[stimmat$line == fix$line[i]][which.min(out)]
      
      fix$trial.nwords[i] <- stimmat$trial.nwords[stimmat$line == fix$line[i]][which.min(out)]
      fix$trial[i] <- stimmat$trial[stimmat$line == fix$line[i]][which.min(out)]
      
      # if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$assign$outlierX) {
      #   
      #   fix$type[i] <- "out"
      #   
      #   fix$line[i] <- NA
      #   fix$letternum[i] <- NA
      #   fix$letter[i] <- NA
      #   fix$wordnum[i] <- NA
      #   fix$word[i] <- NA
      #   fix$sentnum[i] <- NA
      #   fix$sent[i] <- NA
      #   fix$sent.nwords[i] <- NA
      #   fix$ianum[i] <- NA
      #   fix$ia[i] <- NA
      #   
      #   if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
      #     fix$target[i] <- NA
      #   }
      #   
      #   fix$line.let[i] <- NA
      #   fix$word.land[i] <- NA
      #   fix$ia.land[i] <- NA
      #   fix$line.word[i] <- NA
      #   fix$sent.word[i] <- NA
      #   
      #   fix$trial.nwords[i] <- NA
      #   fix$trial[i] <- NA
      #   
      # }
      
    }
    
  }
  
  
  # align fixations on y axis
  # --------------------------
  
  for (i in 1:max(stimmat$line)) {
    fix$ym[fix$line == i & is.na(fix$line) == F] <- stimmat$ym[stimmat$line == i]
  }
  
  
  # return
  # -------
  
  if (env$exp$setup$font$right == T) {
    fix$xs <- env$exp$setup$display$resolutionX - fix$xs
    fix$xn <- env$exp$setup$display$resolutionX - fix$xn
    dat$item[[trial]]$meta$stimmat$xsn <- env$exp$setup$display$resolutionX - dat$item[[trial]]$meta$stimmat$xs
    dat$item[[trial]]$meta$stimmat$xen <- env$exp$setup$display$resolutionX - dat$item[[trial]]$meta$stimmat$xe
    dat$item[[trial]]$meta$stimmat$xs <- dat$item[[trial]]$meta$stimmat$xen
    dat$item[[trial]]$meta$stimmat$xe <- dat$item[[trial]]$meta$stimmat$xsn
    dat$item[[trial]]$meta$stimmat$xsn <- NULL
    dat$item[[trial]]$meta$stimmat$xen <- NULL
    dat$item[[trial]]$meta$stimmat$xm <- (dat$item[[trial]]$meta$stimmat$xs + dat$item[[trial]]$meta$stimmat$xe) / 2
  }
  
  dat$item[[trial]]$fix <- fix[is.na(fix$type) == F, ]  
  
  return(dat)
  
}