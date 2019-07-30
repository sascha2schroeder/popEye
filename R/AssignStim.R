
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
    fix$yn <- fix$ys + (env$exp$setup$display$marginTop - fix$ys[1])
  } else {
    fix$yn <- fix$ys
  }

  
  # define outlier
  # ----------------
  
  fix$type <- "in"

  # fix$type[fix$yn > (max(stimmat$ye) + (stimmat$ye[1] - stimmat$ys[1]) * 2] <- "out"
  # fix$type[fix$yn < (min(stimmat$ys) - (stimmat$ye[1] - stimmat$ys[1]) * 2] <- "out"
  
  
  # line assignment 
  # ----------------
  
  fix$line <- NA
  
  # default method: match single fixations
  if (env$exp$setup$analysis$lineMethod == "match") {
    
    for (i in 1:nrow(fix)) {
      # i <- 2
      
      out <- abs(fix$yn[i] - stimmat$ym)
      
      if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$analysis$outlier.y) {
        fix$type[i] <- "out"
        fix$line[i] <- NA
      } else {
        fix$line[i] <- stimmat$line[which.min(out)]
      }
      
    }
    
  }
  
  # # cluster method
  # if (env$exp$setup$analysis$lineMethod == "cluster") {
  # 
  #   # NOTE: dependency library(fpc)
  # 
  #   if (max(stimmat$line) > 1) {
  # 
  #       clu <- kmeans(fix$yn[fix$type == "in"],
  #                     fpc::pamk(fix$yn[fix$type == "in"],
  #                               criterion="asw",
  #                               krange = 1:max(stimmat$line),
  #                               alpha = .1)$nc)
  #       if (max(clu$cluster) > 1) {
  #         cl_mean <- sort(round(clu$center))
  #         clu <- kmeans(fix$yn[fix$type == "in"], cl_mean)
  #       }
  # 
  #       fix$line[fix$type == "in"] <- clu$cluster
  # 
  #   } else {
  #     
  #     fix$line <- 1
  #     
  #   }
  #   
  # }
  
  # chain method
  if (env$exp$setup$analysis$lineMethod == "chain") {
    
    fix$line[1] <- 1
    fix$distx <- NA
    fix$disty <- NA
    fix$dist <- NA
   
    # assign line number
    for (i in 2:nrow(fix)) {
      fix$distx[i] <- fix$xs[i] - fix$xs[i - 1]
      fix$disty[i] <- fix$ys[i] - fix$ys[i - 1]
      fix$dist[i] <- sqrt(fix$distx[i]^2 + fix$disty[i]^2)
      
      # determine line break
      if (abs(fix$disty[i]) > env$exp$setup$font$height | fix$dist[i] > 20*env$exp$setup$font$height) {
        fix$line[i] <- fix$line[i - 1] + 1
      } else {
        fix$line[i] <- fix$line[i - 1]
      }
      
      # define y outlier
      out <- abs(fix$yn[i] - stimmat$ym)
      
      if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$analysis$outlier.y) {
        fix$type[i] <- "out"
      }
      
    }
    
    # align with line number in stimmat
    linem <- tapply(stimmat$ym, stimmat$line, mean)
    num <- as.numeric(unlist(dimnames(table(fix$line))))
    
    for (i in 1:length(num)) {
      
      out <- vector(length = length(linem))
      for (j in 1:length(linem)) {
        out[j] <- mean((fix$yn[fix$line == num[i]] - linem[j])^2, na.rm = T)
      }
      
      fix$line[fix$line == num[i] & is.na(fix$line) == F] <- which.min(out) 
    }
    
    # fix$distx <- NULL
    # fix$disty <- NULL
    # fix$dist <- NULL
    
  }
  
  # TODO: FixAlign method (Cohen, 2013) 
  
  
  # map letter and IA
  # ------------------
  
  for (i in 1:nrow(fix)) {
    # i <- 1
    
    # out <- vector(length = nrow(stimmat[stimmat$line == fix$line[i], ]))
    # for (j in 1:nrow(stimmat[stimmat$line == fix$line[i], ])) {
    #   # j <- 1
    #   out[j] <- sqrt((fix$xn[i] - mean(c(stimmat$xe[j], stimmat$xs[j])))^2)
    # }
    
    # determine x outlier
    out <- abs(fix$xn[i] - stimmat$xm[stimmat$line == fix$line[i]])
    
    if (out[which.min(out)] > env$exp$setup$font$height * env$exp$setup$analysis$outlier.x) {
      fix$type[i] <- "out"
    }
    
    fix$subid[i] <- stimmat$subid[stimmat$line == fix$line[i]][which.min(out)]
    fix$trialid[i] <- stimmat$trialid[stimmat$line == fix$line[i]][which.min(out)]
    fix$trialnum[i] <- stimmat$trialnum[stimmat$line == fix$line[i]][which.min(out)]
    fix$itemid[i] <- stimmat$itemid[stimmat$line == fix$line[i]][which.min(out)]
    fix$cond[i] <- stimmat$cond[stimmat$line == fix$line[i]][which.min(out)]
    
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
  
  
  # recompute outlier
  # ------------------
  
  fix$line[fix$type == "out"] <- NA
  fix$letternum[fix$type == "out"] <- NA
  fix$letter[fix$type == "out"] <- NA
  fix$wordnum[fix$type == "out"] <- NA
  fix$sentnum[fix$type == "out"] <- NA
  fix$ianum[fix$type == "out"] <- NA
  fix$line.let[fix$type == "out"] <- NA
  fix$word.land[fix$type == "out"] <- NA
  fix$ia.land[fix$type == "out"] <- NA
  
  
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