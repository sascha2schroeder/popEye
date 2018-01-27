
# TODO: specific to experiment -> generalize

ResultsFile <- function(subid, datpath) {
  
  results <- read.table(paste(datpath, "/", subid, "/","results.txt", sep = ""), 
                        header = T, stringsAsFactor = F)
  names <- c("trial_count", "trial_text_rt", "trial_question_resp", 
             "trial_question_acc", "trial_question_rt")
  results <- results[names]  
  colnames(results) <- c("trialnum", "sent.rt", "quest.resp", "quest.acc",
                         "quest.rt")
  
  results$sent.rt <- round(results$sent.rt)
  results$sent.rt[results$sent.rt < 0] <- NA
  results$quest.resp <- as.numeric(results$quest.resp)
  results$quest.acc[is.na(results$quest.resp) == T] <- NA
  results$quest.rt[is.na(results$quest.resp) == T] <- NA
  results$quest.rt <- round(results$quest.rt)
  results$subid <- subid

  names <- c("subid", "trialnum", "sent.rt")
  text <- results[is.na(results$quest.resp) == T, match(names, colnames(results))]
  names <- c("subid", "trialnum", "quest.resp", "quest.acc", "quest.rt")
  quest <- results[is.na(results$quest.resp) == F, match(names, colnames(results))]
  
  
  # save
  out <- list(text = text, quest = quest)
  
  return(out)
  
}
