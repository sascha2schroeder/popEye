
ItemFile <- function(dat, env = parent.frame(n = 1)) {
  
  # create output slot
  item <- data.frame(matrix(NA, 1, 7))
  colnames(item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ia", "word")
  
  # trial loop
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    # temporary item frame
    ia <- unlist(strsplit(dat$trial[[trial]]$meta$text, env$exp$setup$indicator$word))
    ia <- gsub("[[:punct:]]", "", ia)
    itemtmp <- data.frame(matrix(NA, length(ia), 6))
    colnames(itemtmp) <- c("subid", "trialnum", "itemid", "cond", "ia", "word")
    
    itemtmp$trialid <- dat$trial[[trial]]$meta$trialid
    itemtmp$trialnum <- dat$trial[[trial]]$meta$trialnum
    itemtmp$itemid <- dat$trial[[trial]]$meta$itemid
    itemtmp$cond <- dat$trial[[trial]]$meta$cond
    itemtmp$ia <- 1:length(ia)
    itemtmp$word <- ia
    
    # add to output
    item <- rbind(item, itemtmp)
    
  }
  
  # save
  item <- item[-1, ]
  
  return(item)
  
}