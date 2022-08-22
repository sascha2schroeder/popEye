
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
  
  # exp$reports$word.item <- rbind(exp1$reports$word.item, exp2$reports$word.item)
  # exp$reports$ia.item <- rbind(exp1$reports$ia.item, exp2$reports$ia.item)
  # exp$reports$sent.item <- rbind(exp1$reports$sent.item, exp2$reports$sent.item)
  exp$reports$fix <- rbind(exp1$reports$fix, exp2$reports$fix)
  exp$reports$sac <- rbind(exp1$reports$sac, exp2$reports$sac)
  # exp1$reports$clean$transformed <- 0
  # exp2$reports$clean$transformed <- 1
  exp$reports$clean <- rbind(exp1$reports$clean, exp2$reports$clean)
  exp$reports$words <- rbind(exp1$reports$words, exp2$reports$words)
  exp$reports$ias <- rbind(exp1$reports$ias, exp2$reports$ias)
  exp$reports$sentences <- rbind(exp1$reports$sentences, exp2$reports$sentences)
  exp$reports$trials <- rbind(exp1$reports$trials, exp2$reports$trials)
  exp$reports$subjects <- rbind(exp1$reports$subjects, exp2$reports$subjects)
  
  return(exp)
  
}
