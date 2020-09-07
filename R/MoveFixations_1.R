
MoveFixations_1 <- function(fix, stimmat) {
  
  # select fixations
  fixtmp <- fix
  fixtmp <- fixtmp[fixtmp$num > 1, ]
  fixtmp <- fixtmp[, match(c("xn", "yn"), colnames(fixtmp))]
  
  # estimate transformation matrix
  trans_est <- optim(c(0,0), DistFromText, stim=stimmat, fix=fixtmp)$par
  
  # compute relocated fixations
  fixcor <- TranslateXY(trans_est, fixtmp)
  
  # return fixations
  fix$xn[fix$num > 1] <- round(fixcor[,1])
  fix$yn[fix$num > 1] <- round(fixcor[,2])
  
  return(fix)
  
}
