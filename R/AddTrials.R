
AddTrials <- function(exp1, exp2, sub) {

  exp <- exp1
  
  subname <- sub
  subpos <- match(subname, names(exp$subjects))
  trialnames <- names(exp2$subjects[[1]]$trials)
  triallength <- length(exp$subjects[[subpos]]$trials)
  
  for (i in 1:length(trialnames)) {
  
    exp$subjects[[subpos]]$trials[[triallength + i]] <- exp2$subjects[[1]]$trials[[i]]
    names(exp$subjects[[subpos]]$trials)[triallength + i] <- names(exp2$subjects[[1]]$trials)[i]
      
  }
  
  exp$out$fix <- rbind(exp1$out$fix, exp2$out$fix)
  exp$out$fix <- exp$out$fix[order(exp$out$fix$subid, exp$out$fix$trialnum), ]
  exp$out$sac <- rbind(exp1$out$sac, exp2$out$sac)
  exp$out$sac <- exp$out$sac[order(exp$out$sac$subid, exp$out$sac$trialnum), ]
  exp1$out$clean$transformed <- 0
  exp2$out$clean$transformed <- 1
  exp$out$clean <- rbind(exp1$out$clean, exp2$out$clean)
  exp$out$clean <- exp$out$clean[order(exp$out$clean$subid, exp$out$clean$trialnum), ]
  exp$out$words <- rbind(exp1$out$words, exp2$out$words)
  exp$out$words <- exp$out$words[order(exp$out$words$subid, exp$out$words$trialnum), ]
  exp$out$ias <- rbind(exp1$out$ias, exp2$out$ias)
  exp$out$ias <- exp$out$ias[order(exp$out$ias$subid, exp$out$ias$trialnum), ]
  exp$out$sentences <- rbind(exp1$out$sentences, exp2$out$sentences)
  exp$out$sentences <- exp$out$sentences[order(exp$out$sentences$subid, exp$out$sentences$trialnum), ]
  exp$out$trials <- rbind(exp1$out$trials, exp2$out$trials)
  exp$out$trials <- exp$out$trials[order(exp$out$trials$subid, exp$out$trials$trialnum), ]
  
  exp <- AggregateSubjects(exp)
  
  return(exp)
  
}
