
SelectFix <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: compute relative landing position
  
  itmp <- env$itemtmp
  itmp$ind <- paste(env$itemtmp$itemid, env$itemtmp$ia, sep = ":")
  
  # create output slot
  fix <- dat$trial[[1]]$fix[1, ]
  fix$subid <- NA
  fix$trialid <- NA
  fix$trialnum <- NA
  fix$itemid <- NA
  fix$cond <- NA

  # trial loop
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    # temporary fix frame
    fixtmp <- dat$trial[[trial]]$fix
    fixtmp$subid <- "dummy"
    fixtmp$trialid <- dat$trial[[trial]]$meta$trialid
    fixtmp$trialnum <- dat$trial[[trial]]$meta$trialnum
    fixtmp$itemid <- dat$trial[[trial]]$meta$itemid
    fixtmp$cond <- dat$trial[[trial]]$meta$cond
    
    # add to output
    fix <- rbind(fix, fixtmp)
    
    # print(trial)

  }

  # merge stimulus
  fix$ind <- paste(fix$itemid, fix$ia, sep = ":")  
  names <- c("ind", "word")
  itmp2 <- itmp[names]
  colnames(itmp2) <- c("ind", "stimulus")
  fix <- merge(fix, itmp2)
  fix$ind <- NULL
  
  # rename fixid
  fix$fixid <- fix$num
  
  # centered landing position (see Vitu et al., 2001)
  fix$cland <- fix$land - (nchar(fix$stimulus) + 1) / 2
  
  # select variables
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "fixid",  "letter", 
             "word", "ia", "stimulus", "runid", "ia.run", "ia.fix", "ia.run.fix", 
             "dur", "blink", "firstskip", "sac.in", "sac.out", "land", "cland",
             "launch", "refix", "reg.in", "reg.out")
  fix <- fix[names]
  
  return(fix)
  
}
