
AddTrials <- function(exp1, exp2, sub) {

  exp <- exp1
  
  subname <- paste("subject.", sub, sep = "")
  subpos <- match(subname, names(exp$subjects))
  trialnames <- names(exp2$subjects[[1]]$items)
  triallength <- length(exp$subjects[[subpos]]$items)
  
  for (i in 1:length(trialnames)) {
  
    exp$subjects[[subpos]]$items[[triallength + i]] <- exp2$subjects[[1]]$items[[i]]
    names(exp$subjects[[subpos]]$items)[triallength + i] <- names(exp2$subjects[[1]]$items)[i]
      
  }
  
  exp$reports$fix <- rbind(exp1$reports$fix, exp2$reports$fix)
  exp$reports$fix <- exp$reports$fix[order(exp$reports$fix$subid, exp$reports$fix$trialnum), ]
  exp$reports$sac <- rbind(exp1$reports$sac, exp2$reports$sac)
  exp$reports$sac <- exp$reports$sac[order(exp$reports$sac$subid, exp$reports$sac$trialnum), ]
  # exp1$reports$clean$transformed <- 0
  # exp2$reports$clean$transformed <- 1
  exp$reports$clean <- rbind(exp1$reports$clean, exp2$reports$clean)
  exp$reports$clean <- exp$reports$clean[order(exp$reports$clean$subid, exp$reports$clean$trialnum), ]
  exp$reports$words <- rbind(exp1$reports$words, exp2$reports$words)
  exp$reports$words <- exp$reports$words[order(exp$reports$words$subid, exp$reports$words$trialnum), ]
  exp$reports$ias <- rbind(exp1$reports$ias, exp2$reports$ias)
  exp$reports$ias <- exp$reports$ias[order(exp$reports$ias$subid, exp$reports$ias$trialnum), ]
  exp$reports$sentences <- rbind(exp1$reports$sentences, exp2$reports$sentences)
  exp$reports$sentences <- exp$reports$sentences[order(exp$reports$sentences$subid, exp$reports$sentences$trialnum), ]
  exp$reports$trials <- rbind(exp1$reports$trials, exp2$reports$trials)
  exp$reports$trials <- exp$reports$trials[order(exp$reports$trials$subid, exp$reports$trials$trialnum), ]
  
  exp <- AggregateSubjects(exp)
  
  return(exp)
  
}
