
DeleteTrials <- function(exp, sub, trials) {

  subname <- paste("subject.", sub, sep="")
  subpos <- match(subname, names(exp$subjects))
  trialname <- paste("trial.", trials, sep="")
  
  # remove trial slots
  for (i in 1:length(trials)) {
    trialpos <- match(trialname, names(exp$subjects[[subpos]]$trials))
    exp$subjects[[subpos]]$trials[[trialpos[[i]]]] <- NULL
  }
  
  exp$out$word.item <- exp$out$word.item[(is.element(exp$out$word.item$subid, sub) & is.element(exp$out$word.item$itemid, trials)) == F, ]
  exp$out$ia.item <- exp$out$ia.item[(is.element(exp$out$ia.item$subid, sub) & is.element(exp$out$ia.item$itemid, trials)) == F, ]
  exp$out$sent.item <- exp$out$sent.item[(is.element(exp$out$sent.item$subid, sub) & is.element(exp$out$sent.item$itemid, trials)) == F, ]
  exp$out$fix <- exp$out$fix[(is.element(exp$out$fix$subid, sub) & is.element(exp$out$fix$itemid, trials)) == F, ]
  exp$out$sac <- exp$out$sac[(is.element(exp$out$sac$subid, sub) & is.element(exp$out$sac$itemid, trials)) == F, ]
  exp$out$clean <- exp$out$clean[(is.element(exp$out$clean$subid, sub) & is.element(exp$out$clean$itemid, trials)) == F, ]
  exp$out$words <- exp$out$words[(is.element(exp$out$words$subid, sub) & is.element(exp$out$words$itemid, trials)) == F, ]
  exp$out$ias <- exp$out$ias[(is.element(exp$out$ias$subid, sub) & is.element(exp$out$ias$itemid, trials)) == F, ]
  exp$out$sentences <- exp$out$sentences[(is.element(exp$out$sentences$subid, sub) & is.element(exp$out$sentences$itemid, trials)) == F, ]
  exp$out$trials <- exp$out$trials[(is.element(exp$out$trials$subid, sub) & is.element(exp$out$trials$itemid, trials)) == F, ]
  
  exp <- AggregateSubjects(exp)
  
  return(exp)
  
}

