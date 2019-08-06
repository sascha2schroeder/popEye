
AssignStim <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 2
  
  # data
  fix <- dat$trial[[trial]]$fix
  stimmat <- dat$trial[[trial]]$meta$stimmat
  

  # drift correct 
  # ---------------
  
  # x axis
  if (env$exp$setup$analysis$driftX == T) {
    fix$xn <- fix$xs + (env$exp$setup$display$marginLeft - fix$xs[1])
  } else {
    fix$xn <- fix$xs 
  }
  
  # y axis
  if (env$exp$setup$analysis$driftY == T) {
    
    if (fix$ys[1] < 0) {
      fix$yn <- fix$ys
    } else {
      fix$yn <- fix$ys + (env$exp$setup$display$marginTop - fix$ys[1])
    }
  } else {
    fix$yn <- fix$ys
  }

  
  # # define outlier
  # # ----------------
  # 
  # fix$type <- "in"
  # 
  # fix$type[fix$yn > (max(stimmat$ye) + (stimmat$ye[1] - stimmat$ys[1]) * 2)] <- "out"
  # fix$type[fix$yn < (min(stimmat$ys) - (stimmat$ye[1] - stimmat$ys[1]) * 2)] <- "out"
  
  
  # line assignment 
  # ----------------
  
  # default method: match single fixations
  if (env$exp$setup$analysis$lineMethod == "match") {
    
    fix$line <- NA
    
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
    
    # fix$line <- NA
    # fix$linerun <- NA
    # 
    # # first fixation
    # i <- 1
    # stop <- 1
    # while (stop == 1) {
    #   
    #   fix$line[i] <- 1
    #   out <- abs(fix$yn[i] - max(stimmat$ym[stimmat$line == fix$line[i]]))
    #   
    #   if (out > env$exp$setup$font$height * env$exp$setup$analysis$outlierY) {
    #     
    #     out <- abs(fix$yn[i] - stimmat$ym)
    #   
    #     if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$analysis$outlierY) {
    #       fix$type[i] <- "out"
    #       fix$line[i] <- NA
    #       i <- i + 1
    #       next
    #       
    #     } else {
    #       
    #       fix$line[i] <- stimmat$line[which.min(out)]
    #       start <- i
    #       stop <- 0
    #       next
    #       
    #     }
    #     
    #   } else {
    #     
    #     start <- i
    #     stop <- 0
    #     
    #   }
    #   
    # }
    
    # fix$line[start] <- 1
    
    fix$type <- "in"
    fix$linerun <- NA
    fix$linerun[1] <- 1
    fix$line <- NA
    fix$distx <- NA
    fix$disty <- NA
    
    # segment into runs
    # NOTE: loop necessary?
    
    for (i in 2:nrow(fix)) {
      
      fix$distx[i] <- fix$xs[i] - fix$xs[i - 1]
      fix$disty[i] <- fix$ys[i] - fix$ys[i - 1]
      
      # determine line change
      if (abs(fix$disty[i]) >= env$exp$setup$font$height * env$exp$setup$analysis$lineY | 
          abs(fix$distx[i]) >= env$exp$setup$font$height * env$exp$setup$analysis$lineX) {
        fix$linerun[i] <- fix$linerun[i - 1] + 1
      } else {
        fix$linerun[i] <- fix$linerun[i - 1]
      }
      
    }
    
    # align with line number in stimmat
    linem <- tapply(stimmat$ym, stimmat$line, mean)
    num <- as.numeric(unlist(dimnames(table(fix$linerun))))
    
    for (i in 1:length(num)) {
      
      mean.y <- mean(fix$yn[fix$linerun == num[i]], na.rm = T)
      
      if (mean.y > (max(stimmat$ye) + (stimmat$ye[1] - stimmat$ys[1]) * 2) | 
          mean.y < (min(stimmat$ys) - (stimmat$ye[1] - stimmat$ys[1]) * 2)) {
        
        fix$type[fix$linerun == num[i]] <- "out"
        
      } else {
        
        out <- NULL
        
        for (j in 1:length(linem)) {
          out[j] <- (mean.y - linem[j])^2
        }
        
        fix$line[fix$linerun == num[i] & is.na(fix$linerun) == F] <- which.min(out) 
        
      }
        
    }
    
    # fix$distx <- NULL
    # fix$disty <- NULL
    # fix$dist <- NULL
    
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
      
      if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$analysis$outlierX) {
        fix$type[i] <- "out"
        next
      }
      
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
      
    }
    
  }
  
  
  # align fixations on y axis
  # --------------------------
  
  for (i in 1:max(stimmat$line)) {
    fix$ym[fix$line == i & is.na(fix$line) == F] <- stimmat$ym[stimmat$line == i]
  }

  
  # return
  # -------
  
  dat$trial[[trial]]$fix <- fix  
  
  return(dat)
  
}