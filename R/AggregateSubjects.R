
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
    # cagg$ncrit.boundary <- round(cagg$prob.boundary, 3)
    cagg$ncrit.fast <- round(cagg$prob.fast, 3)
    cagg$pcrit <- round(cagg$prob.all, 3)  
  }
  
  # trial
  sagg <- aggregate(cbind(exp$out$trials$nrun, 
                          exp$out$trials$nfix, 
                          exp$out$trials$nblink >= 1, 
                          exp$out$trials$nout >= 1,
                          exp$out$trials$skip, 
                          exp$out$trials$sac, 
                          exp$out$trials$refix, 
                          exp$out$trials$reg, 
                          exp$out$trials$mfix, 
                          exp$out$trials$total, 
                          exp$out$trials$rate),
                    list(exp$out$trials$subid), mean, na.rm = T)
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
  sagg$ntrial <- tapply(exp$out$trials$trialnum, exp$out$trials$subid, length)
  
  # number of exclusions
  sagg$nexc <- sapply(lapply(exp$subjects, "[[", 1), "[[", "exclusion")
  
  # mean calibration accuracy
  sagg$mcal <- sapply(lapply(lapply(lapply(exp$subjects, "[[", 1), "[[", "calibration"), "[[", "avg"), function(x) round(mean(as.numeric(x), na.rm = T), 2))
  # NOTE: weighted mean based on duration of the trial?
  
  # merge and write out
  exp$out$subjects <- merge(cagg, sagg, by = "subid")
  
  names <- c("subid", "ntrial", "nexc", "mcal", "pcrit", "pblink", "pout", "nrun", 
               "nfix", "skip", "sac", "refix", "reg", "mfix", "total", "rate")
  exp$out$subjects <- exp$out$subjects[names]
  row.names(exp$out$subjects) <- NULL
  
  return(exp)
  
}

