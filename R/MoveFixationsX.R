
MoveFixationsX <- function(fix, stimmat, env = parent.frame(n = 1)) {
  
  # compute line coordinates
  stimline <- stimmat[duplicated(stimmat$line) == F, c("line", "xs", "xe", "ys", "ye")]
  stimline$xe <- tapply(stimmat$xe, stimmat$line, max)
  
  mx <- seq(-50, 50, 1)

  out <- matrix(NA, length(mx), 2)

  for (k in 1:length(mx)) {

    out[k, 1] <- mx[k]

    hit <- 0
    for (i in 1:nrow(fix[fix$type == "in", ])) {
      # i <- 1

      for (j in 1:nrow(stimline)) {
        # j <- 1

        if (fix$xn[fix$type == "in"][i] + mx[k] > stimline$xs[j] &
            fix$xn[fix$type == "in"][i] + mx[k] < stimline$xe[j] &
            fix$yn[fix$type == "in"][i] > stimline$ys[j] &
            fix$yn[fix$type == "in"][i] < stimline$ye[j]) {
          hit <- hit + 1
        }

      }

      out[k, 2] <- hit

    }

  }

  env$dat$trial[[env$trial]]$meta$move.x <- out[which.max(out[,2]), 1]
  fix$xn[fix$type == "in"] <- fix$xn[fix$type == "in"] + out[which.max(out[,2]), 1]
  
  return(fix)    
  
}
