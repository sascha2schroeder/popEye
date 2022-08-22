
DeleteTrials <- function(exp, sub, trials) {

  subname <- paste("subject.", sub, sep="")
  subpos <- match(subname, names(exp$subjects))
  trialname <- paste("item.", trials, sep="")
  
  # remove trial slots
  for (i in 1:length(trials)) {
    trialpos <- match(trialname[i], names(exp$subjects[[subpos]]$items))
    exp$subjects[[subpos]]$items[[trialpos]] <- NULL
  }
  
  # exp$reports$word.item <- exp$reports$word.item[(is.element(exp$reports$word.item$subid, sub) & is.element(exp$reports$word.item$itemid, trials)) == F, ]
  # exp$reports$ia.item <- exp$reports$ia.item[(is.element(exp$reports$ia.item$subid, sub) & is.element(exp$reports$ia.item$itemid, trials)) == F, ]
  # exp$reports$sent.item <- exp$reports$sent.item[(is.element(exp$reports$sent.item$subid, sub) & is.element(exp$reports$sent.item$itemid, trials)) == F, ]
  exp$reports$fix <- exp$reports$fix[(is.element(exp$reports$fix$subid, sub) & is.element(exp$reports$fix$itemid, trials)) == F, ]
  exp$reports$sac <- exp$reports$sac[(is.element(exp$reports$sac$subid, sub) & is.element(exp$reports$sac$itemid, trials)) == F, ]
  exp$reports$clean <- exp$reports$clean[(is.element(exp$reports$clean$subid, sub) & is.element(exp$reports$clean$itemid, trials)) == F, ]
  exp$reports$words <- exp$reports$words[(is.element(exp$reports$words$subid, sub) & is.element(exp$reports$words$itemid, trials)) == F, ]
  exp$reports$ias <- exp$reports$ias[(is.element(exp$reports$ias$subid, sub) & is.element(exp$reports$ias$itemid, trials)) == F, ]
  exp$reports$sentences <- exp$reports$sentences[(is.element(exp$reports$sentences$subid, sub) & is.element(exp$reports$sentences$itemid, trials)) == F, ]
  exp$reports$trials <- exp$reports$trials[(is.element(exp$reports$trials$subid, sub) & is.element(exp$reports$trials$itemid, trials)) == F, ]
  
  exp <- AggregateSubjects(exp)
  
  return(exp)
  
}

