
CheckOutlier <- function(fix, stimmat, crit=0.2) {
  
  # compute text field coordinates
  left <- min(stimmat$xs)
  right <- max(stimmat$xe)
  top <- min(stimmat$ys)
  bottom <- max(stimmat$ye)
  
  fix$type <- "in"
  
  fix$type[fix$xn < left - (right - left)*crit] <- "out"
  fix$type[fix$xn > right + (right - left)*crit] <- "out"
  fix$type[fix$yn < top - (bottom - top)*crit] <- "out"
  fix$type[fix$yn > bottom + (bottom - top)*crit] <- "out"
  
  return(fix)
  
}
