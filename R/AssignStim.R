
AssignStim <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 2
  
  # data
  fix <- dat$trial[[trial]]$fix
  stimmat <- dat$trial[[trial]]$meta$stimmat
  

  # align fixations on x axis
  # --------------------------
  
  if (env$exp$setup$analysis$alignX == T) {
    fix$xn <- fix$xs + (env$exp$setup$display$marginLeft - fix$xs[1])
  } else {
    fix$xn <- fix$xs 
  }
  
    
  # align fixations on y axis
  # --------------------------
  
  if (env$exp$setup$analysis$alignY == "drift") {
    fix$yn <- fix$ys + (env$exp$setup$display$marginTop - fix$ys[1])
  } else {
    fix$yn <- fix$ys 
  }
  
  # define outlier
  # ----------------
  
  fix$type <- "in"
  
  # # lose outlier definition
  # fix$type[fix$yn > (max(stimmat$ye) + (stimmat$ye[1] - stimmat$ys[1]) * 2)] <- "out"
  # fix$type[fix$yn < (min(stimmat$ys) - (stimmat$ye[1] - stimmat$ys[1]) * 2)] <- "out"
  
  # strict outlier definition
  fix$type[fix$yn > max(stimmat$ye) + (stimmat$ye[1] - stimmat$ys[1]) * 0.5] <- "out"
  fix$type[fix$yn < min(stimmat$ys) - (stimmat$ye[1] - stimmat$ys[1]) * 0.5] <- "out"
  
  
  # cluster method
  # ---------------
  
  # if (env$exp$setup$analysis$alignY == "cluster") {
  #   # NOTE: dependency library(fpc)
  #   
  #   fix$xn <- fix$xs
  #   
  #   if (max(stimmat$line) > 1) {
  #     clu <- kmeans(fix$ys[fix$type == "in"], 
  #                   fpc::pamk(fix$ys[fix$type == "in"], 
  #                             criterion="asw", 
  #                             krange = 1:max(stimmat$line),
  #                             alpha = .1)$nc)
  #     if (max(clu$cluster) > 1) {
  #       cl_mean <- sort(round(clu$center))
  #       clu <- kmeans(fix$ys[fix$type == "in"], cl_mean)  
  #     }
  #     
  #     fix$cluster[fix$type == "out"] <- 0
  #     fix$cluster[fix$type == "in"] <- clu$cluster
  #     
  #     fix$yn <- fix$ys
  #     line_mean  <- tapply(stimmat$ym, stimmat$line, max)
  #     for(i in 1:max(fix$cluster)) {
  #       fix$yn[fix$cluster == i] <- line_mean[i]
  #     } 
  #     
  #   } else {
  #     
  #     fix$yn <- max(stimmat$ym)
  #     
  #   }
  #   
  # }

  
  # TODO: FixAlign method (Cohen, 2013) 
  
  
  # mapping letter
  # ----------------
  
  for (i in 1:nrow(fix)) {
    # i <- 2
    out <- vector(length = nrow(stimmat))
    for (j in 1:nrow(stimmat)) {
      # j <- 1
      out[j] <- sqrt((fix$xn[i] - mean(c(stimmat$xe[j], stimmat$xs[j])))^2 + (fix$yn[i] - mean(c(stimmat$ye[j], stimmat$ys[j])))^2)
    }
    
    fix$line[i] <- stimmat$line[which.min(out)]
    fix$letternum[i] <- stimmat$letno[which.min(out)]
    fix$letter[i] <- stimmat$letter[which.min(out)]
    fix$wordnum[i] <- stimmat$word[which.min(out)]
    fix$ianum[i] <- stimmat$ia[which.min(out)]
    fix$line.let[i] <- stimmat$letline[which.min(out)]
    fix$word.land[i] <- stimmat$letword[which.min(out)]
    fix$ia.land[i] <- stimmat$letia[which.min(out)]
  }
  
  
  # recompute outlier
  # ------------------
  
  fix$line[fix$type == "out"] <- NA
  fix$letternum[fix$type == "out"] <- NA
  fix$letter[fix$type == "out"] <- NA
  fix$wordnum[fix$type == "out"] <- NA
  fix$ianum[fix$type == "out"] <- NA
  fix$line.let[fix$type == "out"] <- NA
  fix$word.land[fix$type == "out"] <- NA
  fix$ia.land[fix$type == "out"] <- NA
  
  
  # return
  # -------
  
  dat$trial[[trial]]$fix <- fix  
  
  return(dat)
  
}