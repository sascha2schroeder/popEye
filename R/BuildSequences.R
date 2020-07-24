
BuildSequences <- function(fix, env = parent.frame(n = 3)) {
  
  # compute distance
  fix$distx <- NA
  fix$distx[2:length(fix$distx)] <- diff(fix$xn)
  fix$disty <- NA
  fix$disty[2:length(fix$disty)] <- diff(fix$yn)
  
  # initialize variables
  fix$run <- NA
  fix$run[1] <- 1
  
  # segment into runs
  for (i in 2:nrow(fix)) {
    # i <- 2
    
    # determine run break
    if (abs(fix$disty[i]) >= env$exp$setup$font$height * env$exp$setup$analysis$lineY | 
        abs(fix$distx[i]) >= env$exp$setup$font$height * env$exp$setup$analysis$lineX) {
      
      fix$run[i] <- fix$run[i - 1] + 1
      
    } else {
      
      fix$run[i] <- fix$run[i - 1]
      
    }
    
  }
  
  fix$linerun <- fix$run
  fix$linerun[fix$type == "out"] <- NA
  fix$linerun <- as.numeric(as.factor(fix$linerun))
  
  return(fix)
  
}
