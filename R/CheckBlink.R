
# compute blink before/after
# ---------------------------

# NOTE: needs parse/clean as input (not fix)
# NOTE: not used at present; needed in EventLong() and maybe in later steps


CheckBlink <- function(dat) {
  
  dat$blink.before <- 0
  dat$blink.after <- 0
  
  for (i in 1:nrow(tmplong)) {
    if (dat$msg[i] == "BLINK") {
      dat$blink.after[i - 1] <- 1
      if (is.na(dat$msg[i + 1]) == F) {
        dat$blink.before[i + 1] <- 1  
      }
    }
  }
  
  out = dat[, match(c("blink.before", "blink.after"), colnames(dat))]
  return (out)
  
}
