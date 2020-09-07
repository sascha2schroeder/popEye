
# TODO: Make nice

MoveFixations_2 <- function(fix, stimmat, x_adj = F) {
  
  fixtmp <- fix[c("xn", "yn")]
  stim <- stimmat[c("xs","xe", "ys", "ye")]
  
  # compute stimulus box
  lx = min(stim$xs)
  ux = max(stim$xe)
  ly = min(stim$ys)
  uy = max(stim$ye)
  stimpoint <- c(lx, ux, ly, uy)
  
  # compute fixation box
  out <- matrix(NA, nrow(fixtmp), 5)
  
  for (i in 1:nrow(fixtmp)) {
    # i = 1
    tmp <- SmallestBox(fixtmp[-i, ])
    out[i, 1:4] <- tmp$points
    out[i, 5] <- tmp$area
    
  }
  
  fixpoint <- SmallestBox(fixtmp[abs(scale(out[,5])) < 3, ])[[1]]
  
  # move box
  c1 <- matrix(stimpoint, 2, 2)
  c2 <- matrix(fixpoint, 2, 2)
  c3 <- c2
  c3[,1] <- c2[,1] * (c1[2,1] - c1[1,1]) / (c2[2,1] - c2[1,1]) 
  c3[,2] <- c2[,2] * (c1[2,2] - c1[1,2]) / (c2[2,2] - c2[1,2])
  c4 <- c3
  c4[,1] <- c3[,1] - (c3[1,1] - c1[1,1])
  c4[,2] <- c3[,2] - (c3[1,2] - c1[1,2])
  
  # stretch points
  fixcor <- fixtmp
  
  if (x_adj == F) {
    fixcor[,1] <- fixtmp[,1]
  } else {
    # fixcor[,1] <- fixtmpfixcor[,1] <- fixtmp[,1] * (c1[2,1] - c1[1,1]) / (c2[2,1] - c2[1,1])[,1] * (c1[2,1] - c1[1,1]) / (c2[2,1] - c2[1,1])
    fixcor[,1] <- fixtmp[,1] * (c1[2,1] - c1[1,1]) / (c2[2,1] - c2[1,1]) * (c1[2,1] - c1[1,1]) / (c2[2,1] - c2[1,1])
  }
  
  fixcor[,2] <- fixtmp[,2] * (c1[2,2] - c1[1,2]) / (c2[2,2] - c2[1,2])
  
  # translate fixations
  if (x_adj == F) {
    fix$xn <- round(fixcor[,1])
  } else {
    fix$xn <- round(fixcor[,1] - (c3[1,1] - c1[1,1])) 
  }
  
  fix$yn <- round(fixcor[,2] - (c3[1,2] - c1[1,2]))
  
  return(fix)
  
}
