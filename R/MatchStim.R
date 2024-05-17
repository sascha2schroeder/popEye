
MatchStim <- function(dat, trial, env = parent.frame(n = 1)) {
  
  for (trial in 1:length(dat$item)) {
    
    # compute match variable 
    id <- paste(dat$item[[trial]]$meta$itemid, dat$item[[trial]]$meta$condition, sep = ":")
    idnum <- match(id, names(env$exp$setup$stimulus$stimmat))
    dat$item[[trial]]$meta$stimmat <- env$exp$setup$stimulus$stimmat[[idnum]]
    
    # add structural variables
    dat$item[[trial]]$meta$stimmat$subid <- env$subid
    dat$item[[trial]]$meta$stimmat$trialid <- dat$item[[trial]]$meta$trialid
    dat$item[[trial]]$meta$stimmat$trialnum <- dat$item[[trial]]$meta$trialnum
    dat$item[[trial]]$meta$stimmat$itemid <- dat$item[[trial]]$meta$itemid
    dat$item[[trial]]$meta$stimmat$cond <- dat$item[[trial]]$meta$cond
    
    # determine target variable
    if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
      
      dat$item[[trial]]$meta$target <- NA
      
      dat$item[[trial]]$meta$target <- 
        max(dat$item[[trial]]$meta$stimmat$ianum[dat$item[[trial]]$meta$stimmat$target == "n"], na.rm = T)
      
    }
    
    if (env$debug == "line") {
      return (dat)
    }
    
    dat <- AssignStim(dat, trial)
    
  }
  
  # check for empty trials 
  for (i in length(dat$item):1) {
    if (length(dat$item[[i]]$fix) == 0) {
      dat$item[[i]] <- NULL
      env$header$exclusion <- env$header$exclusion + 1
    }
  }
  
 return(dat) 

}