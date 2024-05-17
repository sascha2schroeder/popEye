
AssignIA <- function(dat, trial) {
  # trial <- 2
  
  dat$item[[trial]]$fix$ianum <- 0
  for (j in 1:nrow(dat$item[[trial]]$fix)) {
    # j = 1
    if (is.na(dat$item[[trial]]$fix$letter[j]) == T) next
    for (k in 1:(length(dat$item[[trial]]$meta$ia.boundary) - 1)) {
      # k = 1
      if(dat$item[[trial]]$fix$letter[j] >= dat$item[[trial]]$meta$ia.boundary[k]) {
        dat$item[[trial]]$fix$ianum[j] = k 
      } 
      # print(k)
    }
    # print(j)
  }
  
  return(dat)

}
