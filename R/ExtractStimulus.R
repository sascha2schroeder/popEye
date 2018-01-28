
ExtractStimulus <- function(dat, stimfile, env = parent.frame(n = 2)) {

  # compute indicator variable
  if (is.na(env$exp$setup$stimulus$cond) == TRUE) {
    cond.col <- "cond"
    stimfile$cond <- 1
  } else {
    cond.col <- env$exp$setup$stimulus$cond
  }
  
  stimfile$id <- paste(stimfile[, match(env$exp$setup$stimulus$id, colnames(stimfile))],
                       stimfile[, match(cond.col, colnames(stimfile))], sep = ":")
  
  for (trial in 1:length(dat$trial)) {
    dat$trial[[trial]]$meta$stim <- stimfile[, match(env$exp$setup$stimulus$text, colnames(stimfile))][stimfile[, match("id", colnames(stimfile))] == paste(dat$trial[[trial]]$meta$itemid, dat$trial[[trial]]$meta$cond, sep = ":")]  
    dat$trial[[trial]]$meta$text <- gsub(env$exp$setup$stimulus$target, "", dat$trial[[trial]]$meta$stim)  
    dat$trial[[trial]]$meta$text <- gsub(env$exp$setup$stimulus$word, " ", dat$trial[[trial]]$meta$text)  
    dat$trial[[trial]]$meta$text <- gsub(env$exp$setup$stimulus$ia, "", dat$trial[[trial]]$meta$text)    
  }
  
  # TODO: not sure it makes sense to replace word delimiter with blank here
  # TODO: parse out ia delimiter
  
  
  return(dat)
  
}