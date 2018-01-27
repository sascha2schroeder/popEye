
SelectFix <- function(dat, env = parent.frame(n = 1)) {
  
  # TODO: include word in fixation file
  # TODO: compute relative landing position
  
  # create output slot
  fix <- dat$trial[[1]]$fix[1, ]
  fix$subid <- NA
  fix$trialnum <- NA
  fix$itemid <- NA
  fix$cond <- NA

  # trial loop
  for (trial in 1:length(dat$trial)) {
    # trial = 1
    
    # temporary fix frame
    fixtmp <- dat$trial[[trial]]$fix
    fixtmp$subid <- "dummy"
    fixtmp$trialnum <- dat$trial[[trial]]$meta$trialnum
    fixtmp$itemid <- dat$trial[[trial]]$meta$itemid
    fixtmp$cond <- dat$trial[[trial]]$meta$cond
    
    # add to output
    fix <- rbind(fix, fixtmp)
    
    # print(trial)

  }
  
  # create output file
  fix <- fix[-1, ]
  
  # rename fixid
  fix$fixid <- fix$num
  
  # select variables
  names <- c("subid", "trialnum", "itemid", "cond", "fixid",  "letter", "word",
             "ia", "runid", "ia.run", "ia.fix", "ia.run.fix", "dur", "blink", 
             "firstskip", "sac.in", "sac.out", "land", "launch", "refix", 
             "reg.in", "reg.out")
  fix <- fix[names]
  
  return(fix)
  
}
