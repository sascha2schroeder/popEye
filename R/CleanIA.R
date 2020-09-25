
CleanIA <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: same output format even if no cleaning
  
  trial = 1
  while (trial <= length(dat$item)) {

    if (env$exp$setup$clean$stage3 == T) {
      dat <- CleanStage3(dat, trial)
      # TODO: no merge step implemented at present
    }

    if (env$exp$setup$clean$stage4 == T) {
      dat <- CleanStage4(dat, trial)
    }

    if (env$exp$setup$clean$outlier == TRUE) {
      dat <- CleanOutlier(dat, trial)  
    }

  # recompute itemid
  dat$item[[trial]]$meta$trialid <- trial
  
  # delete trial with empty slots 
  if (is.null(nrow(dat$item[[trial]]$fix))) {
    dat$item[[trial]] <- NULL
    trial <- trial 
  } else {
    trial <- trial + 1
  }
  
}
  
  return(dat)
  
}
