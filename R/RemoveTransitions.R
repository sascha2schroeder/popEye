RemoveTransitions <- function(fix, stimmat) {
  
  crit <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 2
  
  fix$trans <- 0
  for (i in 1:(nrow(fix) - 1)) {
    if (is.na(fix$disty[i]) == T) next
    if (abs(fix$disty[i]) > crit & abs(fix$disty[i + 1]) > crit) {
      fix$trans[i] <- 1
    }
  }
  
  fix$linerun[fix$trans == 1] <- NA
  fix$type[fix$trans == 1] <- "out"
  # fix$trans <- NULL
  
  return(fix)
  
}
