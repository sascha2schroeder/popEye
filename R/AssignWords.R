
AssignWords <- function(dat, trial) {
  # trial <- 2
  dat$trial[[trial]]$fix$word <- 0
  for (j in 1:nrow(dat$trial[[trial]]$fix)) {
    # j = 1
    if (is.na(dat$trial[[trial]]$fix$letter[j]) == T) next
    for (k in 1:(length(dat$trial[[trial]]$meta$word.boundary) - 1)) {
      # k = 1
      if(dat$trial[[trial]]$fix$letter[j] >= dat$trial[[trial]]$meta$word.boundary[k]) {
        dat$trial[[trial]]$fix$word[j] = k 
      } 
      # print(k)
    }
    # print(j)
  }
  
  return(dat)
  
}
