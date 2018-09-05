
ItemFileWord <- function(dat, env = parent.frame(n = 1)) {
  
  # create output slot
  item <- data.frame(matrix(NA, 1, 7))
  colnames(item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "wordnum", "word")
  
  # trial loop
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    # temporary item frame
    word <- unlist(strsplit(dat$trial[[trial]]$meta$stim, env$exp$setup$indicator$word))

    # parse out indicators    
    
    if (env$exp$setup$indicator$ia != " ") {
      word <- gsub(env$exp$setup$indicator$ia, "", word)
    }
    
    word <- gsub(env$exp$setup$indicator$target, "", word)
    # ia <- gsub("[[:punct:]]", "", ia)
    # NOTE: parse out interpunctation?
    
    word <- gsub(env$exp$setup$indicator$line, "", word)
    
    
    itemtmp <- data.frame(matrix(NA, length(word), 6))
    colnames(itemtmp) <- c("subid", "trialnum", "itemid", "cond", "wordnum", "word")
    
    itemtmp$trialid <- dat$trial[[trial]]$meta$trialid
    itemtmp$trialnum <- dat$trial[[trial]]$meta$trialnum
    itemtmp$itemid <- dat$trial[[trial]]$meta$itemid
    itemtmp$cond <- dat$trial[[trial]]$meta$cond
    itemtmp$wordnum <- 1:length(word)
    itemtmp$word <- word
    
    # add to output
    item <- rbind(item, itemtmp)
    
  }
  
  # save
  item <- item[-1, ]
  
  return(item)
  
}