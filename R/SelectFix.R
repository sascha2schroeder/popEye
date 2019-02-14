
SelectFix <- function(dat, env = parent.frame(n = 1)) {
  
  iatmp <- env$ia.itemtmp
  iatmp$ind <- paste(env$ia.itemtmp$itemid, env$ia.itemtmp$ianum, sep = ":")
  
  wordtmp <- env$word.itemtmp
  wordtmp$ind <- paste(env$word.itemtmp$itemid, env$word.itemtmp$wordnum, sep = ":")
  
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

  # remove first row
  fix <- fix[-1, ]
  
  # merge IA
  fix$ind <- paste(fix$itemid, fix$ianum, sep = ":")  
  names <- c("ind", "ia")
  iatmp2 <- iatmp[names]
  fix <- merge(fix, iatmp2, all.x = T)
  fix$ind <- NULL
  
  # merge word
  fix$ind <- paste(fix$itemid, fix$wordnum, sep = ":")  
  names <- c("ind", "word")
  wordtmp2 <- wordtmp[names]
  fix <- merge(fix, wordtmp2, all.x = T)
  fix$ind <- NULL
  
  # rename fixid
  fix$fixid <- fix$num
  
  # centered landing position (see Vitu et al., 2001)
  fix$ia.cland <- fix$ia.land - (nchar(fix$ia) + 1) / 2
  fix$word.cland <- fix$word.land - (nchar(fix$word) + 1) / 2

  # TODO: compute relative landing position
  
  # select variables
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "fixid",  
             "type", "line", "letternum", 
              "letter",
             "ianum", "ia", "wordnum", "word", 
             "line.change", "blink", "dur", "sac.in", "sac.out",
             "ia.runid", "ia.run", "ia.fix", "ia.run.fix", 
             "ia.firstskip",  "ia.land", "ia.cland", "ia.launch", 
             "ia.refix", "ia.reg.in", "ia.reg.out",
             "word.runid", "word.run", "word.fix", "word.run.fix",
             "word.firstskip",  "word.land", "word.cland", "word.launch", 
             "word.refix", "word.reg.in", "word.reg.out")
  fix <- fix[names]
  
  return(fix)
  
}
