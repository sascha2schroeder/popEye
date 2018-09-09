
ItemFileIA <- function(dat, env = parent.frame(n = 1)) {
  
  # create output slot
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    item <- data.frame(matrix(NA, 1, 8))
    colnames(item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia", "target")
  } else {
    item <- data.frame(matrix(NA, 1, 7))
    colnames(item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia")
  }
  
  # trial loop
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    # temporary item frame
    ia <- unlist(strsplit(dat$trial[[trial]]$meta$stim, env$exp$setup$indicator$ia))
    # ia <- gsub("[[:punct:]]", "", ia)
    # NOTE: parse out interpunctation?
    
    # parse out indicators    
    
    ia <- gsub(env$exp$setup$indicator$target, "", ia)
    # ia <- gsub("[[:punct:]]", "", ia)
    # NOTE: parse out interpunctation?
    
    ia <- gsub(env$exp$setup$indicator$line, "", ia)
    
    if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
      itemtmp <- data.frame(matrix(NA, length(ia), 8))
      colnames(itemtmp) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia", "target")
    } else {
      itemtmp <- data.frame(matrix(NA, length(ia), 7))
      colnames(itemtmp) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia")
    }
    
    itemtmp$trialid <- dat$trial[[trial]]$meta$trialid
    itemtmp$trialnum <- dat$trial[[trial]]$meta$trialnum
    itemtmp$itemid <- dat$trial[[trial]]$meta$itemid
    itemtmp$cond <- dat$trial[[trial]]$meta$cond
    itemtmp$ianum <- 1:length(ia)
    itemtmp$ia <- ia
    
    # add target ia
    if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
      itemtmp$target <- 0
      itemtmp$target[itemtmp$ianum == dat$trial[[trial]]$meta$target] = "n"
      itemtmp$target[itemtmp$ianum == dat$trial[[trial]]$meta$target - 1] = "n-1"
      itemtmp$target[itemtmp$ianum == dat$trial[[trial]]$meta$target + 1] = "n+1"
    }
    
    # add to output
    item <- rbind(item, itemtmp)
    
  }
  
  # save
  item <- item[-1, ]
  
  return(item)
  
}