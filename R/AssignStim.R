
AssignStim <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 2
  
  # data
  fix <- dat$trial[[trial]]$fix
  stimmat <- dat$trial[[trial]]$meta$stimmat
  

  # drift correct 
  # ---------------
  
  # x axis
  if (env$exp$setup$analysis$driftX == T) {
    
    if (is.na(dat$trial[[trial]]$meta$drift) == F) {
      fix$xn <- fix$xs - dat$trial[[trial]]$meta$drift.x
    } else {
      fix$xn <- fix$xs 
    }
    
  } else {
    
    fix$xn <- fix$xs 
    
  }
  
  # y axis
  if (env$exp$setup$analysis$driftY == T) {
    
    if (is.na(dat$trial[[trial]]$meta$drift) == F) {
      fix$yn <- fix$ys - dat$trial[[trial]]$meta$drift.y + env$exp$setup$font$height / 2
    } else {
      fix$yn <- fix$ys
    }
    
  } else {
    
    fix$yn <- fix$ys
    
  }
  

  # line assignment 
  # ----------------
  
  # default method: match single fixations
  if (env$exp$setup$analysis$lineMethod == "match") {
    
    fix$type <- "in"
    fix$line <- NA
    fix$linerun <- NA
    
    for (i in 1:nrow(fix)) {
      # i <- 2
      
      out <- abs(fix$yn[i] - stimmat$ym)
      
      if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$analysis$outlierY) {
        fix$type[i] <- "out"
        fix$line[i] <- NA
      } else {
        fix$line[i] <- stimmat$line[which.min(out)]
      }
      
    }
    
  }
  
  
  # cluster method
  if (env$exp$setup$analysis$lineMethod == "cluster") {

    # NOTE: dependency library(fpc)

    if (max(stimmat$line) > 1) {

        clu <- kmeans(fix$yn[fix$type == "in"],
                      fpc::pamk(fix$yn[fix$type == "in"],
                                criterion="asw",
                                krange = 1:max(stimmat$line),
                                alpha = .1)$nc)
        if (max(clu$cluster) > 1) {
          cl_mean <- sort(round(clu$center))
          clu <- kmeans(fix$yn[fix$type == "in"], cl_mean)
        }

        fix$line[fix$type == "in"] <- clu$cluster

    } else {

      fix$line <- 1

    }

  }
 
   
  # chain method
  if (env$exp$setup$analysis$lineMethod == "chain") {
    
    # compute distance
    fix$distx <- NA
    fix$distx[2:length(fix$distx)] <- diff(fix$xn)
    fix$disty <- NA
    fix$disty[2:length(fix$disty)] <- diff(fix$yn)
    
    # compute mean y position of lines
    linem <- tapply(stimmat$ym, stimmat$line, mean)
    
    # initialize variables
    fix$type <- "in"
    fix$linerun <- NA
    fix$linerun[1] <- 1
    fix$line <- NA
    
    mem <- NULL
    start <- 1
    stop <- nrow(linem)
    
    # segment into runs
    for (i in 2:nrow(fix)) {
      
      # determine run break
      if (abs(fix$disty[i]) >= env$exp$setup$font$height * env$exp$setup$analysis$lineY | 
          abs(fix$distx[i]) >= env$exp$setup$font$height * env$exp$setup$analysis$lineX) {
        
        # assign previous run to line
        
        mean.y <- mean(fix$yn[fix$linerun == fix$linerun[i - 1]], na.rm = T)
        
        if (mean.y > (max(stimmat$ye) + (stimmat$ye[1] - stimmat$ys[1]) * 2) | 
            mean.y < (min(stimmat$ys) - (stimmat$ye[1] - stimmat$ys[1]) * 2)) {
          
          fix$type[fix$linerun == fix$linerun[i - 1]] <- "out"
          
        } else {
          
          out <- NULL
          
          for (j in start:stop) {
            out[j] <- (mean.y - linem[j])^2
          }
          
          fix$line[fix$linerun == fix$linerun[i - 1]] <- which.min(out)
          # mem <- which.min(out)
          
        }
        
        # check movement direction 
        
        # NOTE: also determine how many lines have been moved?
        # NOTE: mult als lineY?
        
        # if (fix$disty[i] >= env$exp$setup$font$height * env$exp$setup$analysis$lineY) {
        #   
        #   if (is.null(mem) == F) {
        #     start <- 1
        #   } else {
        #     start <- 1
        #   }
        #   
        #   stop <- nrow(linem)
        #   
        # } else if (fix$disty[i] <= -env$exp$setup$font$height * env$exp$setup$analysis$lineY) {
        #   
        #   start <- mem
        #   stop <- 1
        #   
        # } else {
        #   
        #   # start <- mem
        #   # stop <- mem
        #   
        # }
        
        fix$linerun[i] <- fix$linerun[i - 1] + 1
        
      } else {
        
        fix$linerun[i] <- fix$linerun[i - 1]
        
      }
      
    }
    
    # assign last run
    
    mean.y <- mean(fix$yn[fix$linerun == fix$linerun[nrow(fix)]], na.rm = T)
    
    if (mean.y > (max(stimmat$ye) + (stimmat$ye[1] - stimmat$ys[1]) * 2) | 
        mean.y < (min(stimmat$ys) - (stimmat$ye[1] - stimmat$ys[1]) * 2)) {
      
      fix$type[fix$linerun == fix$linerun[nrow(fix)]] <- "out"
      
    } else {
      
      out <- NULL
      
      for (j in 1:nrow(linem)) {
        out[j] <- (mean.y - linem[j])^2
      }
      
      fix$line[fix$linerun == fix$linerun[nrow(fix)]] <- which.min(out)
      
    }
    
    # fix$distx <- NULL
    # fix$disty <- NULL
    
  }
  
  # TODO: FixAlign method (Cohen, 2013) 
  
  
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
    
    # determine x outlier
    
    if (fix$type[i] == "in") {
      
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
      
      if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$analysis$outlierX) {
        
        fix$type[i] <- "out"
        
        fix$line[i] <- NA
        fix$letternum[i] <- NA
        fix$letter[i] <- NA
        fix$wordnum[i] <- NA
        fix$word[i] <- NA
        fix$sentnum[i] <- NA
        fix$sent[i] <- NA
        fix$sent.nwords[i] <- NA
        fix$ianum[i] <- NA
        fix$ia[i] <- NA
        
        if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
          fix$target[i] <- NA
        }
        
        fix$line.let[i] <- NA
        fix$word.land[i] <- NA
        fix$ia.land[i] <- NA
        fix$line.word[i] <- NA
        fix$sent.word[i] <- NA
        
        fix$trial.nwords[i] <- NA
        fix$trial[i] <- NA
        
      }
      
    }
    
  }
  
  
  # align fixations on y axis
  # --------------------------
  
  for (i in 1:max(stimmat$line)) {
    fix$ym[fix$line == i & is.na(fix$line) == F] <- stimmat$ym[stimmat$line == i]
  }
  
  
  # fit criterion
  # --------------
  
  out <- NULL
  crit.line <- NULL
  
  for (i in 1:nrow(fix)) {
    # i <- 1
    
    # if (fix$type[i] == "in") {
    #   
    #   sel <- stimmat$line == fix$line[i] & stimmat$letternum == fix$letternum[i]
    #   out[i] <- sqrt((fix$xn[i] - stimmat$xm[sel])^2 + (fix$yn[i] - stimmat$ym[sel])^2)
    #   
    # }
    
      out <- abs(fix$yn[i] - stimmat$ym)
      crit.line[i] <- stimmat$line[which.min(out)]
    
  }
  
  # fix$fit <- round(mean(out, na.rm = T), 3)
  fix$fit <- round((length(fix$line[is.na(fix$line) == F]) - sum(fix$line[is.na(fix$line) == F] == crit.line[is.na(fix$line) == F], na.rm = T)) / length(fix$line[is.na(fix$line) == F]), 3)
  
  
  # return
  # -------
  
  dat$trial[[trial]]$fix <- fix  
  
  return(dat)
  
}