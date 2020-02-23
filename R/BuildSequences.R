
BuildSequences <- function(fix, stimmat) {
  
  # initialize variables
  fix$run <- NA
  fix$run[1] <- 1
  
  # parameters
  p1 <- mean(stimmat$width) * 5 # TODO: as parameter
  # p2 <- mean(stimmat$width) * 15 # TODO: as parameter
  # p2 <- mean(stimmat$width) * 20 # TODO: as parameter
  p2 <- mean(stimmat$width) * 25 # TODO: as parameter
  # p3 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 2
  p3 <- (mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 2) * 1.5
  
  # TODO: polygon version with angle, using rectangle for now
  # p4 <- mean((stimmat$ye[stimmat$line == 1] - stimmat$ys[stimmat$line == 1])) / 4
  # # plot polygon
  # x <- c(fix$xn[i] - p1, fix$xn[i] - p1, fix$xn[i] + p2, fix$xn[i] + p2)
  # y <- c(fix$yn[i] + p3, fix$yn[i] - p3, fix$yn[i] - p3 - p4, fix$yn[i] + p3 + p4)
  # polygon(x, y, border = "red")
  
  for (i in 2:nrow(fix)) {
    # i <- 2
    
    if (fix$xn[i] < (fix$xn[i - 1] - p1) | fix$xn[i] > (fix$xn[i - 1] + p2) | 
        fix$yn[i] < fix$yn[i - 1] - p3 | fix$yn[i] > fix$yn[i - 1] + p3) {
      fix$run[i] <- fix$run[i - 1] + 1
    } else {
      fix$run[i] <- fix$run[i - 1]
    }
    
    # print(i)
    
  }
  
  return(fix)
  
}
