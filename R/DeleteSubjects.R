
DeleteSubjects <- function(exp, sub) {

  subname <- paste("subject.", sub, sep="")
  subpos <- match(subname, names(exp$subjects))
  
  # remove subject slot
  for (i in 1:length(subpos)) {
    exp$subjects[[subpos[i]]] <- NULL
  }
  
  exp$out$word.item <- exp$out$word.item[is.element(exp$out$word.item$subid, sub) == F, ]
  exp$out$ia.item <- exp$out$ia.item[is.element(exp$out$ia.item$subid, sub) == F, ]
  exp$out$sent.item <- exp$out$sent.item[is.element(exp$out$sent.item$subid, sub) == F, ]
  exp$out$fix <- exp$out$fix[is.element(exp$out$fix$subid, sub) == F, ]
  exp$out$sac <- exp$out$sac[is.element(exp$out$sac$subid, sub) == F, ]
  exp$out$clean <- exp$out$clean[is.element(exp$out$clean$subid, sub) == F, ]
  exp$out$words <- exp$out$words[is.element(exp$out$words$subid, sub) == F, ]
  exp$out$ias <- exp$out$ias[is.element(exp$out$ias$subid, sub) == F, ]
  exp$out$sentences <- exp$out$sentences[is.element(exp$out$sentences$subid, sub) == F, ]
  exp$out$trials <- exp$out$trials[is.element(exp$out$trials$subid, sub) == F, ]
  exp$out$subjects <- exp$out$subjects[is.element(exp$out$subjects$subid, sub) == F, ]
  
  return(exp)
  
}
