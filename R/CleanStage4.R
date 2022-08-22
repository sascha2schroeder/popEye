
# stage 4 cleaning
# -----------------

# DataViewer description:
# STAGE 4 Data Viewer goes over the entire data and deletes every fixation which
# is shorter than Minimum duration or larger than the maximum duration threshold.
# defaults: minimum = 140, maximum = 800

CleanStage4 <- function(dat, trial, env = parent.frame(n = 2)) {

  dat$item[[trial]]$fix$del <- 0
  
  for (i in 1:nrow(dat$item[[trial]]$fix)) {
    if (dat$item[[trial]]$fix$dur[i] < env$exp$setup$clean$stage4Min) {
      dat$item[[trial]]$fix$del[i] <- 1
      # print(paste("Stage 4: Fixation", i, "too short.", sep = " "))
    } else if (dat$item[[trial]]$fix$dur[i] > env$exp$setup$clean$stage4Max) {
      dat$item[[trial]]$fix$del[i] <- 1
      # print(paste("Stage 4: Fixation", i, "too long.", sep = " "))
    }
  }
  
  dat$item[[trial]]$fix <- 
    dat$item[[trial]]$fix[dat$item[[trial]]$fix$del == 0, ]
  dat$item[[trial]]$fix$del <- NULL
  dat$item[[trial]]$fix$num <- 1:nrow(dat$item[[trial]]$fix) 
  
  return(dat)
  
}
