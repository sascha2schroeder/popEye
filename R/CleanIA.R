
CleanIA <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: common output format even if no cleaning
  
  trial = 1
  while (trial <= length(dat$trial)) {

    # print(trial)
    
    # if (env$exp$setup$clean$stage3 == T) {
    #   dat <- CleanStage3(dat, trial)
    #   # TODO: no merge step implemented at present
    # }
    # 
    # if (env$exp$setup$clean$stage4 == T) {
    #   dat <- CleanStage4(dat, trial)
    # }

    if (env$exp$setup$clean$outlier == T) {
      dat <- CleanOutlier(dat, trial)  
    }

    # if (env$exp$setup$clean$delete == T) {
    #   dat <- DeleteFixations(dat, trial)
    # }

  # recompute itemid
  dat$trial[[trial]]$meta$trialid <- trial
  
  # delete trial with empty slots 
  if (is.null(nrow(dat$trial[[trial]]$fix))) {
    dat$trial[[trial]] <- NULL
    trial <- trial 
  } else {
    trial <- trial + 1
  }
  
}
  
  return(dat)
  
}
