
ComputeOverview <- function(exp) {
  
  if (exp$setup$type == "sentence") {
    cagg=aggregate(cbind(exp$out$clean$trial.crit, exp$out$clean$crit),
                   list(exp$out$clean$subid), mean, na.rm = T)
    colnames(cagg) <- c("subid", "prob.trial", "prob.all")
    cagg$prob.trial <- round(cagg$prob.trial, 3)
    cagg$prob.all <- round(cagg$prob.all, 3)  
  }
  
  if (exp$setup$type == "target") {
    cagg=aggregate(cbind(exp$out$clean$trial.crit, exp$out$clean$target.crit, 
                         exp$out$clean$crit),
                   list(exp$out$clean$subid), mean, na.rm = T)
    colnames(cagg) <- c("subid", "prob.trial", "prob.target", "prob.all")
    cagg$prob.trial <- round(cagg$prob.trial, 3)
    cagg$prob.target <- round(cagg$prob.target, 3)
    cagg$prob.all <- round(cagg$prob.all, 3)  
  }
  
  if (exp$setup$type == "boundary") {
    cagg=aggregate(cbind(exp$out$clean$trial.crit, exp$out$clean$target.crit, 
                         exp$out$clean$boundary.crit, exp$out$clean$crit),
                   list(exp$out$clean$subid), mean, na.rm = T)
    colnames(cagg)=c("subid", "prob.trial", "prob.target", "prob.boundary",
                     "prob.all")
    cagg$prob.trial <- round(cagg$prob.trial, 3)
    cagg$prob.target <- round(cagg$prob.target, 3)
    cagg$prob.boundary <- round(cagg$prob.boundary, 3)
    cagg$prob.all <- round(cagg$prob.all, 3)  
  }
  
  if (exp$setup$type == "fast") {
    cagg=aggregate(cbind(exp$out$clean$trial.crit, exp$out$clean$target.crit, 
                         exp$out$clean$fast.crit, exp$out$clean$crit),
                   list(exp$out$clean$subid), mean, na.rm = T)
    colnames(cagg) <- c("subid", "prob.trial", "prob.target", "prob.fast",
                        "prob.all")
    cagg$prob.trial <- round(cagg$prob.trial, 3)
    cagg$prob.target <- round(cagg$prob.target, 3)
    cagg$prob.fast <- round(cagg$prob.fast, 3)
    cagg$prob.all <- round(cagg$prob.all, 3)  
  }
  
  # trial
  sagg <- aggregate(cbind(exp$out$trial$nrun, exp$out$trial$nfix, 
                          exp$out$trial$blink, exp$out$trial$skip, 
                          exp$out$trial$sac, exp$out$trial$refix, 
                          exp$out$trial$reg, exp$out$trial$mfix, 
                          exp$out$trial$total, exp$out$trial$rate),
                    list(exp$out$trial$subid), mean, na.rm = T)
  colnames(sagg) <- c("subid", "nrun", "nfix", "blink", "skip", "sac", 
                      "refix", "reg", "mfix", "total", "rate")
  
  sagg$nrun <- round(sagg$nrun, 2)
  sagg$nfix <- round(sagg$nfix, 2)
  sagg$blink <- round(sagg$blink, 3)
  sagg$skip <- round(sagg$skip, 3)
  sagg$sac <- round(sagg$sac, 3)
  sagg$refix <- round(sagg$refix, 3)
  sagg$reg <- round(sagg$reg, 3)
  sagg$mfix <- round(sagg$mfix)
  sagg$total <- round(sagg$total)
  sagg$rate <- round(sagg$rate)
  
  # number of trials
  sagg$ntrial <- tapply(exp$out$trial$trialnum, exp$out$trial$subid, length)
  
  # results
  if (exp$setup$tracker$software == "EB") {
    sagg$quest.acc <- round(as.numeric(tapply(exp$out$results$quest$quest.acc, 
                                              exp$out$results$quest$subid, mean)), 3)
    sagg$quest.rt <- round(as.numeric(tapply(exp$out$results$quest$quest.rt, 
                                             exp$out$results$quest$subid, mean)))
  }
  
  # merge and write out
  exp$out$overview <- merge(cagg, sagg, by = "subid")
  
  names <- c("subid", "ntrial", "nrun", "nfix", "blink", "skip", "sac", 
    "refix", "reg", "mfix", "total", "rate", "quest.acc", "quest.rt")
  exp$out$overview <- exp$out$overview[names]
  row.names(exp$out$overview) <- NULL
  
  return(exp)
  
}

