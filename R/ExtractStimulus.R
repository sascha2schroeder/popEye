
ExtractStimulus <- function(dat, stimfile, env = parent.frame(n = 2)) {
  
  # compute condition variable (if not provided)
  if (is.na(env$exp$setup$stimulus$cond) == TRUE) {
    
    # create match variable (itemid)
    stimfile$match <- stimfile[, match(env$exp$setup$stimulus$id, colnames(stimfile))]
    
  } else {
   
    if (length(env$exp$setup$stimulus$cond) == 1) {
      
      # create match variable (itemid:cond)
      stimfile$match <- paste(stimfile[, match(env$exp$setup$stimulus$id, colnames(stimfile))],
                              stimfile[, match(env$exp$setup$stimulus$cond, colnames(stimfile))], sep = ":")
      
    } else if (length(env$exp$setup$stimulus$cond) == 2) {
      
      # create combined condition variable
      cond1 <- env$exp$setup$stimulus$cond[1]
      cond2 <- env$exp$setup$stimulus$cond[2]
      
      stimfile$cond <- paste(stimfile[, match(cond1, colnames(stimfile))],
                             stimfile[, match(cond2, colnames(stimfile))], sep = ":")
                             
      # create match variable (itemid:cond)
      stimfile$match <- paste(stimfile[, match(env$exp$setup$stimulus$id, colnames(stimfile))],
                              stimfile$cond, sep = ":")
     
    }
    
  }
  
  if (is.na(env$exp$setup$stimulus$cond) == TRUE) {
    
    # parse out indicator characters from text display
    for (trial in 1:length(dat$item)) {
      dat$item[[trial]]$meta$stim <- stimfile[, match(env$exp$setup$stimulus$text, colnames(stimfile))][stimfile[, match("match", colnames(stimfile))] == dat$item[[trial]]$meta$itemid]  
      dat$item[[trial]]$meta$text <- gsub(env$exp$setup$indicator$target, "", dat$item[[trial]]$meta$stim)
      if (env$exp$setup$indicator$word != "") {
        dat$item[[trial]]$meta$text <- gsub(env$exp$setup$indicator$word, " ", dat$item[[trial]]$meta$text) 
      }
      dat$item[[trial]]$meta$text <- gsub(env$exp$setup$indicator$line, " ", dat$item[[trial]]$meta$text) 
      if (env$exp$setup$indicator$ia != " ") {
        dat$item[[trial]]$meta$text <- gsub(env$exp$setup$indicator$ia, "", dat$item[[trial]]$meta$text)    
      }
    }
    
    # parse out indicator characters from preview display
    if (env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
      for (trial in 1:length(dat$item)) {
        dat$item[[trial]]$meta$preview <- stimfile[, match(env$exp$setup$stimulus$preview, colnames(stimfile))][stimfile[, match("match", colnames(stimfile))] == dat$item[[trial]]$meta$itemid]  
        dat$item[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$target, "", dat$item[[trial]]$meta$preview)  
        if (env$exp$setup$indicator$word != "") {
          dat$item[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$word, " ", dat$item[[trial]]$meta$preview)  
        }
        dat$item[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$line, " ", dat$item[[trial]]$meta$preview) 
        if (env$exp$setup$indicator$ia != " "){
          dat$item[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$ia, "", dat$item[[trial]]$meta$preview) 
        }
      }  
    }
    
    # parse out indicator characters from prime display
    if (env$exp$setup$type == "fast") {
      for (trial in 1:length(dat$item)) {
        dat$item[[trial]]$meta$prime <- stimfile[, match(env$exp$setup$stimulus$prime, colnames(stimfile))][stimfile[, match("match", colnames(stimfile))] == dat$item[[trial]]$meta$itemid]  
        dat$item[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$target, "", dat$item[[trial]]$meta$prime)  
        if (env$exp$setup$indicator$word != "") {
          dat$item[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$word, " ", dat$item[[trial]]$meta$prime)  
        }
        dat$item[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$line, " ", dat$item[[trial]]$meta$prime)  
        if (env$exp$setup$indicator$ia != " "){
          dat$item[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$ia, "", dat$item[[trial]]$meta$prime)
        }
      }  
    }
    
  } else {
    
    # parse out indicator characters from text display
    for (trial in 1:length(dat$item)) {
      dat$item[[trial]]$meta$stim <- stimfile[, match(env$exp$setup$stimulus$text, colnames(stimfile))][stimfile[, match("match", colnames(stimfile))] == paste(dat$item[[trial]]$meta$itemid, dat$item[[trial]]$meta$cond, sep = ":")]  
      dat$item[[trial]]$meta$text <- gsub(env$exp$setup$indicator$target, "", dat$item[[trial]]$meta$stim)  
      if (env$exp$setup$indicator$word != "") {
        dat$item[[trial]]$meta$text <- gsub(env$exp$setup$indicator$word, " ", dat$item[[trial]]$meta$text)  
      }
      dat$item[[trial]]$meta$text <- gsub(env$exp$setup$indicator$line, " ", dat$item[[trial]]$meta$text)  
      if (env$exp$setup$indicator$ia != " ") {
        dat$item[[trial]]$meta$text <- gsub(env$exp$setup$indicator$ia, "", dat$item[[trial]]$meta$text)
      }
      
    }
    
    # parse out indicator characters from preview display
    if (env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
      for (trial in 1:length(dat$item)) {
        dat$item[[trial]]$meta$preview <- stimfile[, match(env$exp$setup$stimulus$preview, colnames(stimfile))][stimfile[, match("match", colnames(stimfile))] == paste(dat$item[[trial]]$meta$itemid, dat$item[[trial]]$meta$cond, sep = ":")]  
        dat$item[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$target, "", dat$item[[trial]]$meta$preview)  
        if (env$exp$setup$indicator$word != "") {
          dat$item[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$word, " ", dat$item[[trial]]$meta$preview)  
        }
        dat$item[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$line, " ", dat$item[[trial]]$meta$preview)  
        if (env$exp$setup$indicator$ia != " ") {
          dat$item[[trial]]$meta$preview <- gsub(env$exp$setup$indicator$ia, "", dat$item[[trial]]$meta$preview)  
        }
      }  
    }
    
    # parse out indicator characters from prime display
    if (env$exp$setup$type == "fast") {
      for (trial in 1:length(dat$item)) {
        dat$item[[trial]]$meta$prime <- stimfile[, match(env$exp$setup$stimulus$prime, colnames(stimfile))][stimfile[, match("match", colnames(stimfile))] == paste(dat$item[[trial]]$meta$itemid, dat$item[[trial]]$meta$cond, sep = ":")]  
        dat$item[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$target, "", dat$item[[trial]]$meta$prime)  
        if (env$exp$setup$indicator$word != "") {
          dat$item[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$word, " ", dat$item[[trial]]$meta$prime)  
        }
        dat$item[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$line, " ", dat$item[[trial]]$meta$prime) 
        if (env$exp$setup$indicator$ia != " "){
          dat$item[[trial]]$meta$prime <- gsub(env$exp$setup$indicator$ia, "", dat$item[[trial]]$meta$prime) 
        }
      }  
    }
  }
  
  return(dat)
  
}