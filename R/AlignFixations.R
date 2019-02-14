
# stupid first attempt; works only with single-line experiments

AlignFixations <- function(fix){
  fix$ys <- round(mean(fix$ys, na.rm = T))
  return(fix)
}