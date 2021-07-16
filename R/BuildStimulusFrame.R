
BuildStimulusFrame <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # trial <- 13
  
  # retrieve variables
  # ------------------
  
  stim <- trimws(dat$item[[trial]]$meta$stim, which = "both")
  
  x.offset <- env$exp$setup$display$marginLeft
  y.offset <- env$exp$setup$display$marginTop
  line.delim <- env$exp$setup$indicator$line
  letpix <- env$exp$setup$font$letpix
  font.height <- env$exp$setup$font$height
  font.lead <- env$exp$setup$font$lead
  
  # calculate cut-off
  x.cut <- env$exp$setup$display$resolutionX - env$exp$setup$display$marginRight
  
  
  # compute letter
  # ---------------
  
  # parse out ia delimiter and target indicator and split into letters
  tmp_letter <- stim
  tmp_letter <- gsub(env$exp$setup$indicator$target, "", tmp_letter)
  tmp_letter <- gsub(env$exp$setup$indicator$line, "", tmp_letter)
  # tmp <- gsub(env$exp$setup$indicator$word, "", tmp)
  # if (env$exp$setup$indicator$ia != " ") {
  #   tmp <- gsub(env$exp$setup$indicator$ia, "", tmp)
  # }
  
  # TODO: this is only EB behavior; maybe condition on software
  # replace hyphen within word with different character
  tmp_letter <- unlist(strsplit(tmp_letter, " "))
  sel <- grep(".-.", tmp_letter)
  tmp_letter[sel] <- gsub("[-]", "\u20de", tmp_letter[sel])
  tmp_letter <- paste(tmp_letter, collapse = " ")
  
  
  
  
  # compute letters
  letters <- unlist(strsplit(tmp_letter, ""))
 
  tmp_word <- stim
  tmp_word <- gsub(env$exp$setup$indicator$target, "", tmp_word)
  tmp_word <- gsub(env$exp$setup$indicator$line, "", tmp_word)
  if (env$exp$setup$indicator$ia != " ") {
    tmp_word <- gsub(env$exp$setup$indicator$ia, "", tmp_word)
  }
  
  tmp_sent <- stim
  tmp_sent <- gsub(env$exp$setup$indicator$target, "", tmp_sent)
  tmp_sent <- gsub(env$exp$setup$indicator$line, "", tmp_sent)
  tmp_sent <- gsub(env$exp$setup$indicator$word, "", tmp_sent)
  if (env$exp$setup$indicator$ia != " ") {
    tmp_sent <- gsub(env$exp$setup$indicator$ia, "", tmp_sent)
  }
  
  tmp_sent2 <- stim
  tmp_sent2 <- gsub(env$exp$setup$indicator$target, "", tmp_sent2)
  tmp_sent2 <- gsub(env$exp$setup$indicator$line, "", tmp_sent2)
  if (env$exp$setup$indicator$ia != " ") {
    tmp_sent2 <- gsub(env$exp$setup$indicator$ia, "", tmp_sent2)
  }
  
  
  # compute stimmat
  # ---------------
  
  stimmat <- data.frame(matrix(NA, length(letters), 1))
  colnames(stimmat) <- c("subid")
  # TODO: assign screen, assign text
  # TODO: only one letter loop?
  
  # structural variables
  stimmat$subid <- env$subid
  stimmat$trialid <- dat$item[[trial]]$meta$trialid
  stimmat$trialnum <- dat$item[[trial]]$meta$trialnum
  stimmat$itemid <- dat$item[[trial]]$meta$itemid
  stimmat$cond <- dat$item[[trial]]$meta$cond
  
  # letter
  stimmat$letternum <- 1:length(letters)
  stimmat$letter <- letters
  stimmat$width <- NA
  
  # words
  # TODO: punctuation part of word?
  if (env$exp$setup$indicator$word != "") {
    words <- unlist(strsplit(tmp_word, env$exp$setup$indicator$word))
  } else {
    words <- unlist(strsplit(tmp_word, env$exp$setup$separator$word))
  }
  wordnum <- 1
  stimmat$wordnum <- NA
  stimmat$word <- NA
  
  # sentences
  sep_sent <- paste(paste("\\", apply(expand.grid(env$exp$setup$separator$sentence, env$exp$setup$separator$sentence2), 1, paste, collapse = ""), sep = ""), collapse = "|")
  sent <- unlist(strsplit(tmp_sent, sep_sent))
  if (env$exp$setup$indicator$word != "") {
    sent.nwords <- sapply(strsplit(unlist(strsplit(tmp_sent2, sep_sent)), env$exp$setup$indicator$word), length)
  } else {
    sent.nwords <- sapply(strsplit(unlist(strsplit(tmp_sent2, sep_sent)), env$exp$setup$separator$word), length)
  }
  sent.nletters <- sapply(strsplit(unlist(strsplit(tmp_sent, sep_sent)), ""), length)
  sentnum <- 1
  stimmat$sentnum <- NA
  stimmat$sent <- NA
  sentmem <- FALSE
  
  # IA
  if (env$exp$setup$indicator$ia != "") {
    
    tmp_ia <- stim
    tmp_ia <- gsub(env$exp$setup$indicator$target, "", tmp_ia)
    tmp_ia <- gsub(env$exp$setup$indicator$line, "", tmp_ia)
    tmp_ia <- gsub(env$exp$setup$indicator$word, "", tmp_ia)
    
    ias <- unlist(strsplit(tmp_ia, env$exp$setup$indicator$ia))
    ianum <- 1
    stimmat$ianum <- NA
    stimmat$ia <- NA
    mem <- nchar(ias[ianum])
    
  } 
    
  
  # letter loop
  # -------------
  
  i <- 1
  while (i <= nrow(stimmat)) {
    
    # words
    
    # check indicator
    if (is.element(stimmat$letter[i], env$exp$setup$indicator$word)) {
      wordnum <- wordnum + 1
      stimmat <- stimmat[-i, ]
      #i <- i - 1
      next
    }
    
    # check separator
    if (is.element(stimmat$letter[i], env$exp$setup$separator$word)) wordnum <- wordnum + 1
    # TODO: Check hyphens
    if (i > 1) {
      if (stimmat$letter[i - 1] == "\u20de") wordnum <- wordnum + 1
    }
      
    stimmat$wordnum[i] <- wordnum
    stimmat$word[i] <- words[wordnum]
  
    # sentence
    stimmat$sentnum[i] <- sentnum
    sent.let <- unlist(strsplit(sent[sentnum], ""))
    sent.n <- length(sent.let)
    if (sent.n > 20) sent.n <- 20
    stimmat$sent[i] <- paste(sent.let[1:sent.n], collapse = "")
    stimmat$sent.nwords[i] <- sent.nwords[sentnum]
    stimmat$sent.nletters[i] <- sent.nletters[sentnum]
    
    if(is.element(stimmat$letter[i], env$exp$setup$separator$sentence2) & sentmem == TRUE) {
      stimmat$sentnum[i] <- sentnum - 1
    }
    
    # check sentence separator
    if (is.element(stimmat$letter[i], env$exp$setup$separator$sentence)) {
      sentnum <- sentnum + 1
      sentmem <- TRUE
    } else {
      sentmem <- FALSE
    }
    
    # IA
    if (env$exp$setup$indicator$ia == "") {
      
      stimmat$ianum[i] <- stimmat$wordnum[i]
      stimmat$ia[i] <- stimmat$word[i]
      
    } else {
      
      if (stimmat$letternum[i] > mem) {
        ianum <- ianum + 1
        mem <- mem + nchar(ias[ianum])
      }
      
      stimmat$ianum[i] <- ianum
      ia.let <- unlist(strsplit(ias[ianum], ""))
      ia.n <- length(ia.let)
      if (ia.n > 20) ia.n <- 20
      stimmat$ia[i] <- paste(ia.let[1:ia.n], collapse = "")
      
    }
    
    # compute width
    if (env$exp$setup$font$fixed == FALSE) {
      
      if (is.element(stimmat$letter[i], letpix$letter) == F) {
        print(paste("Letter", stimmat$letter[i], "missing.", sep = " "))
      }
      stimmat$width[i] <- letpix$pixel[letpix$letter == stimmat$letter[i]]
      
    } else {
      
      if (is.element(stimmat$letter[i], env$exp$setup$font$half) == T) {
        weight <- .5
      } else {
        weight <- 1
      }
      stimmat$width[i] <- letpix$pixel[1]*weight
      
    }
    
    i <- i + 1
    
  }
    
  stimmat$letternum <- 1:nrow(stimmat)
  row.names(stimmat) <- NULL
  
  
  # compute initial x positions
  # ----------------------------
  stimmat$xs <- c(x.offset, cumsum(stimmat$width) + x.offset)[1:length(stimmat$width)]
  stimmat$xe <- cumsum(stimmat$width) + x.offset
  # NOTE: seperate start and end positions necessary?
  
  
  # compute lines
  # --------------
  
  # NOTE: maybe condition on software (ET vs EB)
  
  stimmat$line <- 1
  
  # Stage 1: compute manual line breaks
  
  if (grepl(line.delim, stim)) {
    
    # TODO: move up?
    tmp_line <- stim
    tmp_line <- gsub(env$exp$setup$indicator$target, "", tmp_line)
    tmp_line <- gsub(env$exp$setup$indicator$word, "", tmp_line)
    if (env$exp$setup$indicator$ia != " ") {
      tmp_line <- gsub(env$exp$setup$indicator$ia, "", tmp_line)
    }
    
    line.length <- sapply(unlist(strsplit(tmp_line, line.delim)), nchar)

    nlines <- length(line.length)
    
    
    # line loop
    for (n in 1:(nlines - 1)) {
      # n <- 1
      
      # line.length <- sapply(unlist(strsplit(tmp, line.delim)), nchar)
      
      if (line.length[n] == 0) next 
      
      # stimmat$line[stimmat$line == n  & stimmat$letternum >= cumsum(line.length)[n] + (n*-1) + 2] <- stimmat$line[stimmat$line == n & stimmat$letternum >= line.length[n]] + 1
      stimmat$line[stimmat$line == n  & stimmat$letternum > cumsum(line.length)[n]] <- n + 1
      
      # recompute x positions
      stimmat$xs[stimmat$line == n + 1] <- c(x.offset, cumsum(stimmat$width[stimmat$line == n + 1]) + x.offset)[1:length(stimmat$width[stimmat$line == n + 1])]
      stimmat$xe[stimmat$line == n + 1] <- cumsum(stimmat$width[stimmat$line == n + 1]) + x.offset
      
    }
    
    for (n in 1:nlines) {
      
      if (stimmat$letter[min(stimmat$letternum[stimmat$line == n])] == " ") {
        
        # delete blank at begin of line
        stimmat <- stimmat[-min(stimmat$letternum[stimmat$line == n]), ]
        
        # recompute letternum
        stimmat$letternum <- 1:nrow(stimmat)
        
        # recompute x positions
        stimmat$xs[stimmat$line == n] <- c(x.offset, cumsum(stimmat$width[stimmat$line == n]) + x.offset)[1:length(stimmat$width[stimmat$line == n])]
        stimmat$xe[stimmat$line == n] <- cumsum(stimmat$width[stimmat$line == n]) + x.offset
        
      }
      
    }
    
  }
  
  # Stage 2: compute wrap-up line breaks
  
  n <- 1; m <- 1
  while (n == m) {
    
    # env$exp$setup$font$wrap <- F
   
    if (env$exp$setup$font$wrap == T & length(stimmat$wordnum[stimmat$line == n]) > 0) {
      
      # set line break
      line.cut <- max(stimmat$wordnum[stimmat$line == n & stimmat$xe <= x.cut])
      
      if (stimmat$letter[max(stimmat$letternum[stimmat$wordnum == line.cut])] == "\u20de") {
        line.cut <- line.cut + 1
      }   
      
      if (max(stimmat$xe[stimmat$line == n]) > x.cut) {
        
        # set new line
        stimmat$line[stimmat$wordnum >= line.cut] <- stimmat$line[stimmat$wordnum >= line.cut] + 1
        
        # delete blank before line break
        if (stimmat$letter[min(stimmat$letternum[stimmat$line == n + 1]) - 1] != "\u20de") {
            stimmat <- stimmat[-min(stimmat$letternum[stimmat$line == n + 1]), ]
        }
        
        # recompute letter number
        stimmat$letternum <- 1:nrow(stimmat)
        
        # recompute x positions
        stimmat$xs[stimmat$line == n + 1] <- c(x.offset, cumsum(stimmat$width[stimmat$line == n + 1]) + x.offset)[1:length(stimmat$width[stimmat$line == n + 1])]
        stimmat$xe[stimmat$line == n + 1] <- cumsum(stimmat$width[stimmat$line == n + 1]) + x.offset
        
      }
      
    } else {
      
      # set line break
      line.cut <- max(stimmat$letternum[stimmat$line == n & stimmat$xe <= x.cut])
      
      if (max(stimmat$xe[stimmat$line == n]) > x.cut) {
        
        # set new line
        stimmat$line[stimmat$letternum > line.cut] <- stimmat$line[stimmat$letternum > line.cut] + 1
        
        # recompute x positions
        stimmat$xs[stimmat$line == n + 1] <- c(x.offset, cumsum(stimmat$width[stimmat$line == n + 1]) + x.offset)[1:length(stimmat$width[stimmat$line == n + 1])]
        stimmat$xe[stimmat$line == n + 1] <- cumsum(stimmat$width[stimmat$line == n + 1]) + x.offset
        
      }
      
    }
    
    if (max(stimmat$line) > n) {
      m <- m + 1
    }
    
    n <- n + 1
    
  }
  
  
  # compute y positions
  # --------------------
  
  for (i in 1:max(stimmat$line)) {
    # i = 1
    
    stimmat$ys[stimmat$line == i] <- y.offset + font.lead + (font.height + 1) * (i - 1) + (font.height) * as.numeric(env$exp$setup$font$spacing) * (i - 1) - font.height * 0.5
    stimmat$ye[stimmat$line == i] <- y.offset + font.lead + (font.height + 1) * (i - 1) + (font.height) * as.numeric(env$exp$setup$font$spacing) * (i - 1) + font.height * 1.5
    
    # print (i)
  }
  
  
  # compute mean positions
  # -----------------------
  
  stimmat$xm <- (stimmat$xs + stimmat$xe) / 2
  stimmat$ym <- (stimmat$ys + stimmat$ye) / 2

  
  # reconstruct hyphens
  # --------------------
  
  stimmat$letter <- gsub("\u20de", "-", stimmat$letter)
  
    
  # determine target IA
  # --------------------
  
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    
    stimmat$target <- NA
    if (env$exp$setup$indicator$ia != "") {
      dat$item[[trial]]$meta$target <- 
        grep(env$exp$setup$indicator$target, unlist(strsplit(stim, env$exp$setup$indicator$ia)))
    } else {
      dat$item[[trial]]$meta$target <- 
        grep(env$exp$setup$indicator$target, unlist(strsplit(stim, env$exp$setup$separator$word)))
    }
    
    stimmat$target[stimmat$ianum == dat$item[[trial]]$meta$target] <- "n"
    stimmat$target[stimmat$ianum == dat$item[[trial]]$meta$target - 1] <- "n-1"
    stimmat$target[stimmat$ianum == dat$item[[trial]]$meta$target + 1] <- "n+1"
    
  }
  
  
  # number of words
  # ----------------
  
  stimmat$trial.nwords <- length(words)
  stimmat$trial.nletters <- nrow(stimmat)
  stimmat$trial <- paste(words[1:5], collapse = " ")
  
  
  # letter positions
  # ----------------
  
  # letter in line
  stimmat$letline <- ave(stimmat$letternum, stimmat$line, FUN = rank)
  
  # letter in word
  stimmat$letword <- ave(stimmat$letternum, stimmat$wordnum, FUN = rank)
  for (i in 1:max(stimmat$wordnum)) {
    # i <- 1
    if (is.element(stimmat$letter[stimmat$wordnum == i & stimmat$letword == 1], env$exp$setup$separator$word)) {
      stimmat$letword[stimmat$wordnum == i] <- stimmat$letword[stimmat$wordnum == i] - 1
    }
  }
  
  # letter in IA
  stimmat$letia <- ave(stimmat$letternum, stimmat$ianum, FUN = rank)
  for (i in 1:max(stimmat$ianum)) {
    # i <- 1
    if (is.element(stimmat$letter[stimmat$ianum == i & stimmat$letia == 1], env$exp$setup$separator$word)) {
      stimmat$letia[stimmat$ianum == i] <- stimmat$letia[stimmat$ianum == i] - 1
    }
  }
  
  
  # word positions
  # ---------------
  
  wordmat <- stimmat[duplicated(stimmat$wordnum) == F, ]
  
  # word in line
  wordmat$wordline <- ave(wordmat$wordnum, wordmat$line, FUN = rank)
  
  # word in sentence
  wordmat$wordsent <- ave(wordmat$wordnum, wordmat$sentnum, FUN = rank)
  
  names <- c("wordnum", "wordline", "wordsent")
  stimmat <- merge(stimmat, wordmat[names])
  
  
  # return
  # -------
  
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    
    names <- c("subid", "trialid", "trialnum", "itemid", "cond", "trial", "trial.nwords",
               "letternum", "letter", "wordnum", "word", "sentnum", "sent", "sent.nwords",
               "ianum", "ia", "target", "width", "line", 
               "xs", "xe", "ys", "ye", "xm", "ym",
               "letline", "letword", "letia", "wordline", "wordsent")
  } else {
    
    names <- c("subid", "trialid", "trialnum", "itemid", "cond", "trial", "trial.nwords",
               "letternum", "letter", "wordnum", "word", "sentnum", "sent", "sent.nwords",
               "ianum", "ia", "width", "line", 
               "xs", "xe", "ys", "ye", "xm", "ym",
               "letline", "letword", "letia", "wordline", "wordsent")
  }
  
  dat$item[[trial]]$meta$stimmat <- stimmat
  
  return(dat)
  
}
