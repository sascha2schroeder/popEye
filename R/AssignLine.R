
AssignLine <- function(fix) {
  
  mrun <- tapply(fix$yn, fix$run, mean)
  lrun <- unlist(dimnames(sort(mrun)))
  
  fix$line <- NA
  for (i in 1:length(lrun)) {
    fix$line[fix$run == lrun[i]] <- i
  }
  
  return(fix)
  
}
