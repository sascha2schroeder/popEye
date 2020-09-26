
AggregateSubjects <- function(exp) {
  
  if (exp$setup$type == "text") {
    cagg=aggregate(cbind(exp$out$clean$trial.crit, exp$out$clean$crit),
                   list(exp$out$clean$subid), mean, na.rm = T)
    colnames(cagg) <- c("subid", "prob.trial", "prob.all")
    cagg$ncrit.trial <- round(cagg$prob.trial, 3)
    cagg$pcrit <- round(cagg$prob.all, 3)  
  }
  
  if (exp$setup$type == "sentence") {
    cagg=aggregate(cbind(exp$out$clean$trial.crit, exp$out$clean$crit),
                   list(exp$out$clean$subid), mean, na.rm = T)
    colnames(cagg) <- c("subid", "prob.trial", "prob.all")
    cagg$ncrit.trial <- round(cagg$prob.trial, 3)
    cagg$pcrit <- round(cagg$prob.all, 3)  
  }
  
  if (exp$setup$type == "target") {
    cagg=aggregate(cbind(exp$out$clean$trial.crit, exp$out$clean$target.crit, 
                         exp$out$clean$crit),
                   list(exp$out$clean$subid), mean, na.rm = T)
    colnames(cagg) <- c("subid", "prob.trial", "prob.target", "prob.all")
    cagg$ncrit.trial <- round(cagg$prob.trial, 3)
    cagg$ncrit.target <- round(cagg$prob.target, 3)
    cagg$pcrit <- round(cagg$prob.all, 3)  
  }
  
  if (exp$setup$type == "boundary") {
    cagg=aggregate(cbind(exp$out$clean$trial.crit, exp$out$clean$target.crit, 
                         exp$out$clean$boundary.crit, exp$out$clean$crit),
                   list(exp$out$clean$subid), mean, na.rm = T)
    colnames(cagg)=c("subid", "prob.trial", "prob.target", "prob.boundary",
                     "prob.all")
    cagg$ncrit.trial <- round(cagg$prob.trial, 3)
    cagg$ncrit.target <- round(cagg$prob.target, 3)
    cagg$ncrit.boundary <- round(cagg$prob.boundary, 3)
    cagg$pcrit <- round(cagg$prob.all, 3)  
  }
  
  if (exp$setup$type == "fast") {
    cagg=aggregate(cbind(exp$out$clean$trial.crit, exp$out$clean$target.crit, 
                         exp$out$clean$fast.crit, exp$out$clean$crit),
                   list(exp$out$clean$subid), mean, na.rm = T)
    colnames(cagg) <- c("subid", "prob.trial", "prob.target", "prob.fast",
                        "prob.all")
    cagg$ncrit.trial <- round(cagg$prob.trial, 3)
    cagg$ncrit.target <- round(cagg$prob.target, 3)
    cagg$ncrit.boundary <- round(cagg$prob.boundary, 3)
    cagg$ncrit.fast <- round(cagg$prob.fast, 3)
    cagg$pcrit <- round(cagg$prob.all, 3)  
  }
  
  # trial
  sagg <- aggregate(cbind(exp$out$trial$nrun, 
                          exp$out$trial$nfix, 
                          exp$out$trial$nblink >= 1, 
                          exp$out$trial$nout >= 1,
                          exp$out$trial$skip, 
                          exp$out$trial$sac, 
                          exp$out$trial$refix, 
                          exp$out$trial$reg, 
                          exp$out$trial$mfix, 
                          exp$out$trial$total, 
                          exp$out$trial$rate),
                    list(exp$out$trial$subid), mean, na.rm = T)
  colnames(sagg) <- c("subid", "nrun", "nfix", "nblink", "nout", 
                      "skip", "sac", "refix", "reg", "mfix", "total", 
                      "rate")
  
  sagg$nrun <- round(sagg$nrun, 2)
  sagg$nfix <- round(sagg$nfix, 2)
  sagg$pblink <- round(sagg$nblink, 3)
  sagg$pout <- round(sagg$nout, 3)
  sagg$skip <- round(sagg$skip, 3)
  sagg$sac <- round(sagg$sac, 3)
  sagg$refix <- round(sagg$refix, 3)
  sagg$reg <- round(sagg$reg, 3)
  sagg$mfix <- round(sagg$mfix)
  sagg$total <- round(sagg$total)
  sagg$rate <- round(sagg$rate)
  
  # number of trials
  sagg$ntrial <- tapply(exp$out$trial$trialnum, exp$out$trial$subid, length)
  
  # number of exclusions
  sagg$nexc <- sapply(lapply(exp$subjects, "[[", 1), "[[", "exclusion")
  
  # mean calibration accuracy
  sagg$mcal <- sapply(lapply(lapply(lapply(exp$subjects, "[[", 1), "[[", "calibration"), "[[", "avg"), function(x) round(mean(as.numeric(x), na.rm = T), 2))
  # NOTE: weighted mean based on duration of the trial?
  
  # results
  if (exp$setup$tracker$software == "EB" & exp$setup$tracker$results == T) {
    sagg$quest.acc <- round(as.numeric(tapply(exp$out$results$quest$quest.acc, 
                                              exp$out$results$quest$subid, mean, na.rm = T)), 3)
    sagg$quest.rt <- round(as.numeric(tapply(exp$out$results$quest$quest.rt, 
                                             exp$out$results$quest$subid, mean, na.rm = T)))
  }
  
  # merge and write out
  exp$out$subjects <- merge(cagg, sagg, by = "subid")
  
  if (exp$setup$tracker$software == "EB" & exp$setup$tracker$results == T) {
    names <- c("subid", "ntrial", "nexc", "mcal", "pcrit", "pblink", "pout", "nrun", 
               "nfix", "skip", "sac", "refix", "reg", "mfix", "total", "rate", 
               "quest.acc", "quest.rt")
  } else {
    names <- c("subid", "ntrial", "nexc", "mcal", "pcrit", "pblink", "pout", "nrun", 
               "nfix", "skip", "sac", "refix", "reg", "mfix", "total", "rate")
  }
  
  exp$out$subjects <- exp$out$subjects[names]
  row.names(exp$out$subjects) <- NULL
  
  return(exp)
  
}

