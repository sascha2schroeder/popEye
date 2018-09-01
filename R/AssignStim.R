
AssignStim <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 2
  
  # data
  fix <- dat$trial[[trial]]$fix
  stimmat <- dat$trial[[trial]]$meta$stimmat
  
  # define outlier
  fix$type <- "in"
  fix$type[fix$ys > (max(stimmat$ye) + (stimmat$ye[1] - stimmat$ys[1]) * 2)] <- "out"
  fix$type[fix$ys < (min(stimmat$ys) - (stimmat$ye[1] - stimmat$ys[1]) * 2)] <- "out"
  

  # align fixations on x axis
  # --------------------------
  
  # no corretion
  if (env$exp$setup$analysis$alignX == "no") {
    fix$xn <- fix$xs 
  }
  
  # drift method
  if (env$exp$setup$analysis$alignX == "drift") {
    fix$xn <- fix$xs + (env$exp$setup$display$marginLeft - fix$xs[1])
  } 
  
    
  # align fixations on y axis
  # --------------------------
  
  # no corretion
  if (env$exp$setup$analysis$alignY == "no") {
    fix$yn <- fix$ys 
  }
  
  # drift correct method
  if (env$exp$setup$analysis$alignY == "drift") {
    fix$yn <- fix$ys + (env$exp$setup$display$marginTop - fix$ys[1])
  }
  
  # cluster method
  if (env$exp$setup$analysis$alignY == "cluster") {
    # NOTE: dependency library(fpc)
    
    fix$xn <- fix$xs
    
    if (max(stimmat$line) > 1) {
      clu <- kmeans(fix$ys[fix$type == "in"], 
                    fpc::pamk(fix$ys[fix$type == "in"], 
                              criterion="asw", 
                              krange = 1:max(stimmat$line),
                              alpha = .1)$nc)
      if (max(clu$cluster) > 1) {
        cl_mean <- sort(round(clu$center))
        clu <- kmeans(fix$ys[fix$type == "in"], cl_mean)  
      }
      
      fix$cluster[fix$type == "out"] <- 0
      fix$cluster[fix$type == "in"] <- clu$cluster
      
      fix$yn <- fix$ys
      line_mean  <- tapply(stimmat$ym, stimmat$line, max)
      for(i in 1:max(fix$cluster)) {
        fix$yn[fix$cluster == i] <- line_mean[i]
      } 
      
    } else {
      
      fix$yn <- max(stimmat$ym)
      
    }
    
  }

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
    
  }
  
  # recompute outlier
  fix$letternum[fix$type == "out"] <- NA
  fix$letter[fix$type == "out"] <- NA
  fix$wordnum[fix$type == "out"] <- NA
  fix$ianum[fix$type == "out"] <- NA
  fix$line[fix$type == "out"] <- 0
  
  # return
  dat$trial[[trial]]$fix <- fix
  
  return(dat)
  
}