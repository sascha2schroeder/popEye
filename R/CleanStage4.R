
# stage 4 cleaning
# -----------------

# DataViewer description:
# STAGE 4 Data Viewer goes over the entire data and deletes every fixation which
# is shorter than Minimum duration or larger than the maximum duration threshold.
# defaults: minimum = 140, maximum = 800

CleanStage4 <- function(dat, trial, env = parent.frame(n = 2)) {

  dat$trial[[trial]]$fix$del <- 0
  
  for (i in 1:nrow(dat$trial[[trial]]$fix)) {
    if (dat$trial[[trial]]$fix$dur[i] < env$exp$setup$clean$stage4Min) {
      dat$trial[[trial]]$fix$del[i] <- 1
      # print(paste("Stage 4: Fixation", i, "too short.", sep = " "))
    } else if (dat$trial[[trial]]$fix$dur[i] > env$exp$setup$clean$stage4Max) {
      dat$trial[[trial]]$fix$del[i] <- 1
      # print(paste("Stage 4: Fixation", i, "too long.", sep = " "))
    }
  }
  
  dat$trial[[trial]]$fix <- 
    dat$trial[[trial]]$fix[dat$trial[[trial]]$fix$del == 0, ]
  dat$trial[[trial]]$fix$del <- NULL
  dat$trial[[trial]]$fix$num <- 1:nrow(dat$trial[[trial]]$fix) 
  
  return(dat)
  
}
