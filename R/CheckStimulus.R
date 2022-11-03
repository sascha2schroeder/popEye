CheckStimulus <- function(exp, cond = F) {
  
  # TODO: conditions
  
  stimlist <- exp$setup$stimulus$stimmat
  
  match <- names(stimlist)
  itemid <- sapply(strsplit(match, ":"), "[[", 1)
  cond <- sapply(strsplit(match, ":"), "[[", 2)
  
  # if (is.na(exp$setup$variable$cond) == T) {
  #   matchred <- match[duplicated(itemid) == F]
  # } else {
  #   matchred <- match
  # }
  
  if (cond == F) {
    matchred <- match[duplicated(itemid) == F]
  } else {
    matchred <- match
  }
  
  
  for (i in matchred) {
    PlotStimulus2(stimlist[[i]], interactive = T)
  }
  
}
