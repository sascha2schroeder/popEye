
ExtractStimulus <- function(dat, stimfile, env = parent.frame(n = 2)) {
  
  # NOTE: minimal: itemid, (condition), text
  # NOTE: needed: word marker, IA marker, boundary marker, target marker
  
  col.itemid <- "itemid"
  col.text <- "sentence"
  col.id <- "id"
  # TODO: as variables in call?
  
    
  # compute indicator variable
  if (is.na(env$exp$setup$message$condition) == TRUE) {
    stimfile$cond <- 1
  } 
  stimfile$id <- paste(stimfile$itemid, stimfile$cond, sep = ":")
  
  for (trial in 1:length(dat$trial)) {
    
    # sentence
    if (env$exp$setup$type == "sentence" | env$exp$setup$type == "target" |
        env$exp$setup$type == "boundary") {
      dat$trial[[trial]]$meta$text <- stimfile[, match(col.text, colnames(stimfile))][stimfile[, col.id] == paste(dat$trial[[trial]]$meta$itemid, dat$trial[[trial]]$meta$cond, sep = ":")]  
    }
    
    #   # target 
    #   if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary") {
    #     if (is.na(pos.target) == T) {
    #       dat$trial[[trial]]$stim$target <- stimfile[trial, col.target]
    #       # NOTE: if provided as separate column  
    #     } else {
    #       dat$trial[[trial]]$stim$target <- pos.target  
    #     }
    #   }
    #   # TODO: compute from position in sentence or provide as seperate column?
    #   # NOTE: assumes EB format at present
    #   
    #   # boundary
    #   if (env$exp$setup$type == "boundary") {
    #     dat$trial[[trial]]$stim$boundary <- 
    #       gsub(",0)", "", gsub("[(]", "", stimfile[trial, col.boundary]))
    #   }
    #   # TODO: compute from target position or provide as column?
    #   # NOTE: assumes EB format at present
    #   # TODO: in pixel or in letters ?
    #   
  }
  
  return(dat)
  
}