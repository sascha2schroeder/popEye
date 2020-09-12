
AddSubjects <- function(exp1, exp2) {

  length1 <- length(exp1$subjects)
  length2 <- length(exp2$subjects)
  
  names1 <- names(exp1$subjects)
  names2 <- names(exp2$subjects)
  
  exp <- exp1

  for (i in 1:length2){
    exp$subjects[[length1 + i]] <- exp2$subjects[[i]]
    names(exp$subjects)[length1 + i] <- names(exp2$subjects)[i]
  }
  
  exp$out$word.item <- rbind(exp1$out$word.item, exp2$out$word.item)
  exp$out$ia.item <- rbind(exp1$out$ia.item, exp2$out$ia.item)
  exp$out$sent.item <- rbind(exp1$out$sent.item, exp2$out$sent.item)
  exp$out$fix <- rbind(exp1$out$fix, exp2$out$fix)
  exp$out$sac <- rbind(exp1$out$sac, exp2$out$sac)
  # exp1$out$clean$transformed <- 0
  # exp2$out$clean$transformed <- 1
  exp$out$clean <- rbind(exp1$out$clean, exp2$out$clean)
  exp$out$words <- rbind(exp1$out$words, exp2$out$words)
  exp$out$ias <- rbind(exp1$out$ias, exp2$out$ias)
  exp$out$sentences <- rbind(exp1$out$sentences, exp2$out$sentences)
  exp$out$trials <- rbind(exp1$out$trials, exp2$out$trials)
  exp$out$subjects <- rbind(exp1$out$subjects, exp2$out$subjects)
  
  return(exp)
  
}
