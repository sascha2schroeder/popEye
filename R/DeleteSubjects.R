
DeleteSubjects <- function(exp, sub) {

  subname <- paste("subject.", sub, sep="")
  subpos <- match(subname, names(exp$subjects))
  
  # remove subject slot
  for (i in 1:length(subpos)) {
    exp$subjects[[subpos[i]]] <- NULL
  }
  
  # exp$reports$word.item <- exp$reports$word.item[is.element(exp$reports$word.item$subid, sub) == F, ]
  # exp$reports$ia.item <- exp$reports$ia.item[is.element(exp$reports$ia.item$subid, sub) == F, ]
  # exp$reports$sent.item <- exp$reports$sent.item[is.element(exp$reports$sent.item$subid, sub) == F, ]
  exp$reports$fix <- exp$reports$fix[is.element(exp$reports$fix$subid, sub) == F, ]
  exp$reports$sac <- exp$reports$sac[is.element(exp$reports$sac$subid, sub) == F, ]
  exp$reports$clean <- exp$reports$clean[is.element(exp$reports$clean$subid, sub) == F, ]
  exp$reports$words <- exp$reports$words[is.element(exp$reports$words$subid, sub) == F, ]
  exp$reports$ias <- exp$reports$ias[is.element(exp$reports$ias$subid, sub) == F, ]
  exp$reports$sentences <- exp$reports$sentences[is.element(exp$reports$sentences$subid, sub) == F, ]
  exp$reports$trials <- exp$reports$trials[is.element(exp$reports$trials$subid, sub) == F, ]
  exp$reports$subjects <- exp$reports$subjects[is.element(exp$reports$subjects$subid, sub) == F, ]
  
  return(exp)
  
}
