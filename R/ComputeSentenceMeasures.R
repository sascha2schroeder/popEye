
ComputeSentenceMeasures <- function(fix, sent.item) {
  
  # NOTE: necessary to recompute sentnum and runid due to exception rule
  # NOTE: Be careful: Regressions etc. on fixation-level due not correspond to sentence-level measures anymore
  
   fixin <- fix[fix$type == "in", ]
  
   
   # compute sentence measures
   # -------------------------
    
   # recompute sentence number (two fixation exception rule)
   fixin$sentnum2 <- fixin$sentnum
   
   for (i in 1:max(fixin$trialid)) {
     # i <- 1
     
     for (j in 2:(nrow(fixin[fixin$trialid == i, ]) - 2)) {
       # j <- 13
       
       if (fixin$sentnum2[fixin$trialid == i][j] != fixin$sentnum2[fixin$trialid == i][j - 1]) {
         
         if (fixin$sentnum2[fixin$trialid == i][j + 1] == fixin$sentnum2[fixin$trialid == i][j - 1] |  
             fixin$sentnum2[fixin$trialid == i][j + 2] == fixin$sentnum2[fixin$trialid == i][j - 1]) {
           fixin$sentnum2[fixin$trialid == i][j] <- fixin$sentnum2[fixin$trialid == i][j - 1]
         }
         
       } 
       
     }
     
   }

  # compute indicator
  fixin$id <- as.character(paste(fixin$trialid, fixin$sentnum2, sep = ":"))
  
  # recompute measures
  
  fixin$last <- NA
  
  fixin$sent.reg.in2 <- 0
  fixin$sent.reg.out2 <- 0
  fixin$sent.reg.in.from2  <- NA
  fixin$sent.reg.out.to2 <- NA
  
  fixin$sent.runid2 <- 1
  
  fixin$forward <- 0
  fixin$forward[1] <- 1
  
  for (i in 1:max(fixin$trialid)) {
    # i <- 1
    
    fixin$last[fixin$trialid == i][1] <- fixin$id[fixin$trialid == i][1]
    
    for (j in 2:nrow(fixin[fixin$trialid == i, ])) {
      # j <- 1
    
      # compute last
      fixin$last[fixin$trialid == i][j] <- fixin$id[fixin$trialid == i][j - 1]
      
      
      # compute regressions
      if(fixin$sentnum2[fixin$trialid == i][j] < fixin$sentnum2[fixin$trialid == i][j - 1]) {
        
        fixin$sent.reg.in2[fixin$trialid == i][j] <- 1
        fixin$sent.reg.out2[fixin$trialid == i][j - 1] <- 1
        fixin$sent.reg.in.from2[fixin$trialid == i][j] <- fixin$sentnum2[fixin$trialid == i][j - 1]
        fixin$sent.reg.out.to2[fixin$trialid == i][j - 1] <- fixin$sentnum2[fixin$trialid == i][j]
        
      }
      
      # compute runid
      if (fixin$sent.reg.in2[fixin$trialid == i][j] == 1 & fixin$sent.reg.in2[fixin$trialid == i][j - 1] != 1) {
        fixin$sent.runid2[fixin$trialid == i][j] <- fixin$sent.runid2[fixin$trialid == i][j - 1] + 1
      } else {
        fixin$sent.runid2[fixin$trialid == i][j] <- fixin$sent.runid2[fixin$trialid == i][j - 1]
      }
      
      # compute forward
      if (fixin$wordnum[fixin$trialid == i][j] >= fixin$wordnum[fixin$trialid == i][j-1] ) {
        fixin$forward[fixin$trialid == i][j] <- 1
      }
      
    }
    
  }
  
  # id with run
  fixin$id2 <- paste(fixin$id, fixin$sent.runid2, sep = ":")
  
  # recompute sent.run
  fixtmp <- fixin[duplicated(fixin$id2) == F, ]
  fixtmp$sent.run2 <- ave(fixtmp$sent.runid2, fixtmp$id, FUN = rank)
  fixin <- merge(fixin, fixtmp[c("id2", "sent.run2")], by = "id2")
  fixin <- fixin[order(fixin$trialid, fixin$fixid), ]
  
  # compute firstpass
  fixin$firstpass <- 1
  fixin$firstpass[fixin$sent.run2 > 1] <- 0

  # create sentence outfile  
  sent <- fixin[duplicated(fixin$id) == F, ]
  names <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "sentnum2", "sent", "sent.nwords")
  sent <- sent[names]  
  colnames(sent) <- c("id", "subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "sent", "sent.nwords")
  
  # compute firstrun skip
  sent$firstrun.skip <- 0
  
  for (i in 1:max(sent$trialid)) {
    # i <- 10
    
    mem <- NULL
    
    for (j in 1:nrow(sent[sent$trialid == i, ])) {
      # j = 1
      
      if (is.null(mem) == F) {
        
        if (is.null(mem) == F & sent$sentnum[sent$trialid == i][j] < max(mem)) {
          sent$firstrun.skip[sent$trialid == i][j] <- 1
        }
        
      }
      
      if (sent$sentnum[sent$trialid == i][j] %in% mem == F) {
        mem <- c(mem, sent$sentnum[sent$trialid == i][j])
      }
      
    }
    
  }
  
  # total
  tmp <- aggregate(fixin$dur, list(fixin$id), length)
  colnames(tmp) <- c("id", "total.nfix")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$total.nfix[is.na(sent$total.nfix)] <- 0
  
  tmp <- aggregate(fixin$dur, list(fixin$id), sum)
  colnames(tmp) <- c("id", "total.dur")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$total.dur[is.na(sent$total.dur)] <- 0
  
  # firstpass
  tmp <- aggregate(fixin$dur[fixin$firstpass == 1], list(fixin$id[fixin$firstpass == 1]), length)
  colnames(tmp) <- c("id", "firstpass.nfix")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$firstpass.nfix[is.na(sent$firstpass.nfix)] <- 0
  
  tmp <- aggregate(fixin$dur[fixin$firstpass == 1], list(fixin$id[fixin$firstpass == 1]), sum)
  colnames(tmp) <- c("id", "firstpass.dur")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$firstpass.dur[is.na(sent$firstpass.dur)] <- 0
  
  # firstpass-forward
  tmp <- aggregate(fixin$dur[fixin$firstpass == 1 & fixin$forward == 1], list(fixin$id[fixin$firstpass == 1 & fixin$forward == 1]), length)
  colnames(tmp) <- c("id", "firstpass.forward.nfix")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$firstpass.forward.nfix[is.na(sent$firstpass.forward.nfix)] <- 0
  
  tmp <- aggregate(fixin$dur[fixin$firstpass == 1 & fixin$forward == 1], list(fixin$id[fixin$firstpass == 1 & fixin$forward == 1]), sum)
  colnames(tmp) <- c("id", "firstpass.forward.dur")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$firstpass.forward.dur[is.na(sent$firstpass.forward.dur)] <- 0
  
  # firstpass-reread
  tmp <- aggregate(fixin$dur[fixin$firstpass == 1 & fixin$forward == 0], list(fixin$id[fixin$firstpass == 1 & fixin$forward == 0]), length)
  colnames(tmp) <- c("id", "firstpass.reread.nfix")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$firstpass.reread.nfix[is.na(sent$firstpass.reread.nfix)] <- 0
  
  tmp <- aggregate(fixin$dur[fixin$firstpass == 1 & fixin$forward == 0], list(fixin$id[fixin$firstpass == 1 & fixin$forward == 0]), sum)
  colnames(tmp) <- c("id", "firstpass.reread.dur")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$firstpass.reread.dur[is.na(sent$firstpass.reread.dur)] <- 0
  
  if(sum(fixin$firstpass == 0) != 0) {
    
    # lookback
    tmp <- aggregate(fixin$dur[fixin$firstpass == 0], list(fixin$id[fixin$firstpass == 0]), length)
    colnames(tmp) <- c("id", "lookback.nfix")
    sent <- merge(sent, tmp, by = "id", all.x = T)
    sent$lookback.nfix[is.na(sent$lookback.nfix)] <- 0
    
    tmp <- aggregate(fixin$dur[fixin$firstpass == 0], list(fixin$id[fixin$firstpass == 0]), sum)
    colnames(tmp) <- c("id", "lookback.dur")
    sent <- merge(sent, tmp, by = "id", all.x = T)
    sent$lookback.dur[is.na(sent$lookback.dur)] <- 0
  
    # lookfrom
    fixin$id2 <- paste(fixin$id, fixin$sent.runid2, sep = ":")
    sent2 <- fixin[duplicated(fixin$id2) == F, ]
    sent3 <- sent2[sent2$firstpass == 0 & is.na(sent2$sent.reg.in.from) == F, ]
    
    tmp <- aggregate(fixin$dur[fixin$id2 %in% sent3$id2], list(fixin$id2[fixin$id2 %in% sent3$id2]), length)
    colnames(tmp) <- c("id2", "lookfrom.nfix")
    tmp2 <- merge(tmp, sent3)
    tmp3 <- aggregate(tmp2$lookfrom.nfix, list(tmp2$last), sum)
    colnames(tmp3) <- c("last", "lookfrom.nfix")
    sent <- merge(sent, tmp3, by.x = "id", , by.y = "last", all.x = T)
    sent$lookfrom.nfix[is.na(sent$lookfrom.nfix)] <- 0
    
    tmp <- aggregate(fixin$dur[fixin$id2 %in% sent3$id2], list(fixin$id2[fixin$id2 %in% sent3$id2]), sum)
    colnames(tmp) <- c("id2", "lookfrom.dur")
    tmp2 <- merge(tmp, sent3)
    tmp3 <- aggregate(tmp2$lookfrom.dur, list(tmp2$last), sum)
    colnames(tmp3) <- c("last", "lookfrom.dur")
    sent <- merge(sent, tmp3, by.x = "id", , by.y = "last", all.x = T)
    sent$lookfrom.dur[is.na(sent$lookfrom.dur)] <- 0
    
    
  } else {
    
    sent$lookback.nfix <- 0
    sent$lookback.dur <- 0
    sent$lookfrom.nfix <- 0
    sent$lookfrom.dur <- 0
    
  }


  # firstrun
  # ---------
  
  firstruntmp <- fixin[fixin$sent.run2 == 1, ]
  
  tmp <- aggregate(firstruntmp$sent.reg.in2, list(firstruntmp$id), max)
  colnames(tmp) <- c("id", "firstrun.reg.in")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$firstrun.reg.in[is.na(sent$firstrun.reg.in)] <- 0
  
  tmp <- aggregate(firstruntmp$sent.reg.out2, list(firstruntmp$id), max)
  colnames(tmp) <- c("id", "firstrun.reg.out")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  sent$firstrun.reg.out[is.na(sent$firstrun.reg.out)] <- 0
  
  
  # complete sentence
  # -----------------
  
  # gopast time
  gopasttmp <- fixin
  gopasttmp$sentnum <- gopasttmp$sentnum2
  tmp <- ComputeGopastSentence(gopasttmp)
  names <- c("id", "gopast", "selgopast")
  tmp <- tmp[names]
  tmp <- tmp[duplicated(tmp$id) == F, ]
  colnames(tmp) <- c("id", "gopast", "gopast.sel")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  
  # nrun
  tmp <- aggregate(fixin$sent.run2, list(fixin$id), max)
  colnames(tmp) <- c("id", "nrun")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  
  # reread
  sent$reread <- ifelse(sent$nrun > 1, 1, 0)
  
  # reg.in
  tmp <- aggregate(fixin$sent.reg.in2, list(fixin$id), max)
  colnames(tmp) <- c("id", "reg.in")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  
  # reg.out
  tmp <- aggregate(fixin$sent.reg.out2, list(fixin$id), max)
  colnames(tmp) <- c("id", "reg.out")
  sent <- merge(sent, tmp, by = "id", all.x = T)
  
  # rate
  sent$rate <- round(60000 / (sent$total.dur / sent$sent.nwords))
  
  
  # write out
  # ---------
  
  # delete variables
  sent <- sent[, -match(c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "sent"), colnames(sent))]
  
  item <- sent.item
  item$id <- as.character(paste(item$trialid, item$sentnum, sep = ":"))
  sent <- merge(sent, item, by = "id", all.y = T)
  sent$skip <- 0
  sent$skip[is.na(sent$nrun) == T] <- 1
  
  # save
  names <- c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "sent",
             "sent.nwords", "skip", "nrun", "reread", "reg.in", "reg.out",
             "total.nfix", "total.dur", "rate",
             "gopast", "gopast.sel",
             "firstrun.skip", "firstrun.reg.in", "firstrun.reg.out",
             "firstpass.nfix", "firstpass.dur",
             "firstpass.forward.nfix", "firstpass.forward.dur", 
             "firstpass.reread.nfix", "firstpass.reread.dur",
             "lookback.nfix", "lookback.dur",
             "lookfrom.nfix", "lookfrom.dur")
  
  sent <- sent[names]
  colnames(sent) <- c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "sent",
                      "sent.nwords", "skip", "nrun", "reread", "reg.in", "reg.out",
                      "total.nfix", "total.dur", "rate",
                      "gopast", "gopast.sel",
                      "firstrun.skip", "firstrun.reg.in", "firstrun.reg.out",
                      "firstpass.nfix", "firstpass.dur",
                      "firstpass.forward.nfix", "firstpass.forward.dur", 
                      "firstpass.reread.nfix", "firstpass.reread.dur",
                      "lookback.nfix", "lookback.dur",
                      "lookfrom.nfix", "lookfrom.dur")
  sent <- sent[order(sent$trialid, sent$sentnum), ]
  
  return(sent)
  
}
