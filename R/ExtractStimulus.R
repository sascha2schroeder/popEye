
ExtractStimulus <- function(dat, stimfile, env = parent.frame(n = 2)) {

  # compute condition variable (if not provided)
  if (is.na(env$exp$setup$stimulus$cond) == TRUE) {
    cond.col <- "cond"
    stimfile$cond <- 1
  } else {
    cond.col <- env$exp$setup$stimulus$cond
  }
  
  # create match variable (itemid:cond)
  stimfile$match <- paste(stimfile[, match(env$exp$setup$stimulus$id, colnames(stimfile))],
                       stimfile[, match(cond.col, colnames(stimfile))], sep = ":")
  
  # parse out indicator characters from text display
  for (trial in 1:length(dat$trial)) {
    dat$trial[[trial]]$meta$stim <- stimfile[, match(env$exp$setup$stimulus$text, colnames(stimfile))][stimfile[, match("match", colnames(stimfile))] == paste(dat$trial[[trial]]$meta$itemid, dat$trial[[trial]]$meta$cond, sep = ":")]  
    dat$trial[[trial]]$meta$text <- gsub(env$exp$setup$indicator$target, "", dat$trial[[trial]]$meta$stim)  
    dat$trial[[trial]]$meta$text <- gsub(env$exp$setup$indicator$word, " ", dat$trial[[trial]]$meta$text)  
    dat$trial[[trial]]$meta$text <- gsub(env$exp$setup$indicator$ia, "", dat$trial[[trial]]$meta$text)    
  }
  
  # parse out indicator characters from preview display
  if (env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    for (trial in 1:length(dat$trial)) {
      dat$trial[[trial]]$meta$preview <- stimfile[, match(env$exp$setup$stimulus$preview, colnames(stimfile))][stimfile[, match("match", colnames(stimfile))] == paste(dat$trial[[trial]]$meta$itemid, dat$trial[[trial]]$meta$cond, sep = ":")]  
      dat$trial[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$target, "", dat$trial[[trial]]$meta$preview)  
      dat$trial[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$word, " ", dat$trial[[trial]]$meta$preview)  
      dat$trial[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$ia, "", dat$trial[[trial]]$meta$preview)    
    }  
  }
  
  # parse out indicator characters from prime display
  if (env$exp$setup$type == "fast") {
    for (trial in 1:length(dat$trial)) {
      dat$trial[[trial]]$meta$prime <- stimfile[, match(env$exp$setup$stimulus$prime, colnames(stimfile))][stimfile[, match("match", colnames(stimfile))] == paste(dat$trial[[trial]]$meta$itemid, dat$trial[[trial]]$meta$cond, sep = ":")]  
      dat$trial[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$target, "", dat$trial[[trial]]$meta$prime)  
      dat$trial[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$word, " ", dat$trial[[trial]]$meta$prime)  
      dat$trial[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$ia, "", dat$trial[[trial]]$meta$prime)    
    }  
  }
  
  return(dat)
  
}