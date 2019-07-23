
ItemFileIA <- function(dat, env = parent.frame(n = 1)) {
  
  # create output slot
  
  item <- data.frame(matrix(NA, 1, 8))
  colnames(item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", 
                      "ianum", "ia")
  
  # trial loop
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    itemtmp <- 
      dat$trial[[trial]]$meta$stimmat[duplicated(dat$trial[[trial]]$meta$stimmat$ianum) == F, ]
    
    names <- c("subid", "trialid", "trialnum", "itemid", 
               "cond", "sentnum", "ianum", "ia")
    itemtmp <- itemtmp[names]
    
    # add to output
    item <- rbind(item, itemtmp)
    
  }
  
  # save
  item <- item[-1, ]
  
  return(item)
  
}