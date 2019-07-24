
BuildStimulusFrame <- function(dat, trial, env = parent.frame(n = 2)) {
  
  
  # retrieve variables
  # ------------------
  
  stim <- dat$trial[[trial]]$meta$stim
  
  x.res <- env$exp$setup$display$resolutionX
  x.offset <- env$exp$setup$display$marginLeft
  y.offset <- env$exp$setup$display$marginTop
  right.limit <- env$exp$setup$display$marginRight
  line.delim <- env$exp$setup$indicator$line
  letpix <- env$exp$setup$font$letpix
  font.height <- env$exp$setup$font$height
  font.lead <- env$exp$setup$font$lead
  
  # calculate cut-off
  x.cut <- x.res - right.limit
  
  # set spacing multiplier
  if (env$exp$setup$font$spacing == "1") mult <- 1
  if (env$exp$setup$font$spacing == "1.5") mult <- 1.5
  if (env$exp$setup$font$spacing == "2") mult <- 2
  # TODO: other spacing conditions
  
  
  # compute letter
  # ---------------
  
  # parse out ia delimiter and target indicator and split into letters
  tmp <- stim
  tmp <- gsub(env$exp$setup$indicator$target, "", tmp)
  tmp <- gsub(env$exp$setup$indicator$line, "", tmp)
  if (env$exp$setup$indicator$ia != " ") {
    tmp <- gsub(env$exp$setup$indicator$ia, "", tmp)
  }
  
  letters <- unlist(strsplit(tmp, ""))
  
  
  # set up stimmat matrix
  # ----------------------
  
  stimmat <- data.frame(matrix(NA, length(letters), 1))
  colnames(stimmat) <- c("subid")
  # TODO: assign screen, assign text
  # TODO: only one letter loop?
  
  stimmat$letternum <- 1:length(letters)
  stimmat$letter <- letters
  
  
  # add other variables
  # -------------------
  
  stimmat$subid <- env$subid
  stimmat$trialid <- dat$trial[[trial]]$meta$trialid
  stimmat$trialnum <- dat$trial[[trial]]$meta$trialnum
  stimmat$itemid <- dat$trial[[trial]]$meta$itemid
  stimmat$cond <- dat$trial[[trial]]$meta$cond
  
  
  # replace hyphens
  # ----------------
  
  # NOTE: hyphens work as word seperator
  hyph <- grep("-", stimmat$letter)
  if (length(hyph) > 0) {
    stimmat$letter[hyph] <- " "
    tmp <- gsub("-", " ", tmp)
  }
  
    
  # compute word
  # -------------
  
  word.delim <- "[ -]"
  # TODO: word.delim as parameter
  # TODO: punctuation part of word?
  
  words <- unlist(strsplit(tmp, word.delim))
  wordnum <- 1
  for (i in 1:length(letters)) {
    if (stimmat$letter[i] == " ") wordnum <- wordnum + 1
    stimmat$wordnum[i] <- wordnum
    stimmat$word[i] <- words[stimmat$wordnum[i]]
    stimmat$width[i] <- letpix$pixel[letpix$letter == stimmat$letter[i]]
    # print(i)
  }
  
  
  # sentence
  # ---------
  
  sent.sep <- ".?!"
  # TODO: as parameter?
  
  delim <- unlist(strsplit(sent.sep, ""))
  sent.delim <- " "
  for (i in 1:length(delim)) {
    sent.delim <- c(sent.delim, paste(delim[i], " ", sep = ""))
  }
  sent.delim <- sent.delim[-1]
  sent.bound <- unlist(strsplit(sent.sep, ""))
  
  sentnum <- 1
  stimmat$sentnum[1] <- 1
  sent <- unlist(strsplit(tmp, paste(paste("[", sent.sep, "]", sep = ""), " ", sep = "")))
  
  for (i in 1:(length(letters) - 1)) {
    if (is.element(stimmat$letter[i], sent.bound) & stimmat$letter[i + 1] == " ") {
      sentnum <- sentnum + 1
    }
    stimmat$sentnum[i + 1] <- sentnum
    stimmat$sent[i] <- paste(unlist(strsplit(sent[stimmat$sentnum[i]], ""))[1:20], collapse = "")
    stimmat$sent.nwords[i] <- sapply(strsplit(sent[stimmat$sentnum[i]], " "), length)
    stimmat$sent.nletters[i] <- sapply(strsplit(sent[stimmat$sentnum[i]], ""), length)
    # print(i)
  }
  
  
  # compute IA
  # -----------
  
  ia.delim <- env$exp$setup$indicator$ia
  
  if (ia.delim == " ") {
    
    stimmat$ianum <- stimmat$wordnum
    stimmat$ia <- stimmat$word
    
  } else {
    
    tmp <- stim
    tmp <- gsub("-", " ", tmp)
    tmp <- gsub(env$exp$setup$indicator$target, "", tmp)
    tmp <- gsub(env$exp$setup$indicator$line, "", tmp)
    
    ias <- unlist(strsplit(tmp, ia.delim))
    ianum <- 1
    mem <- nchar(ias[ianum])
    
    for (i in 1:nrow(stimmat)) {
      
      if (stimmat$letternum[i] > mem) {
        ianum <- ianum + 1
        mem <- mem + nchar(ias[ianum])
      }
      
      stimmat$ianum[i] <- ianum
      ia.let <- unlist(strsplit(ias[stimmat$ianum[i]], ""))
      ia.n <- length(ia.let)
      if (ia.n > 20) ia.n <- 20
      stimmat$ia[i] <- paste(ia.let[1:ia.n], collapse = "")
      # print(i)
      
    }
    
  }

  
  # compute initial x positions
  # ----------------------------
  stimmat$xs <- c(x.offset, cumsum(stimmat$width) + x.offset)[1:length(stimmat$width)]
  stimmat$xe <- cumsum(stimmat$width) + x.offset
  # NOTE: seperate start and end positions necessary?

  
  # compute lines
  # --------------
  
  stimmat$line <- 1
  
  # NOTE: does not work if there is a mixture between explicit and implicit
  # line breaks
  
  # NOTE: maybe condition on software (ET vs EB)
  
  # Stage 1: compute wrap-up line breaks
  
  if (length(grep(line.delim, stim)) > 0) {
    
    tmp <- stim
    tmp <- gsub(env$exp$setup$indicator$target, "", tmp)
    tmp <- gsub(env$exp$setup$indicator$ia, "", tmp)
    
    line.length <- sapply(unlist(strsplit(tmp, line.delim)), nchar)
    
    nlines <- length(line.length)
    
    # line loop
    for (n in 1:(nlines - 1)) {
      
      if (line.length[n] == 0) next 
      
      stimmat$line[stimmat$line == n  & stimmat$letternum >= line.length[n]] <- stimmat$line[stimmat$line == n & stimmat$letternum >= line.length[n]] + 1
      
      # delete blank before line break
      stimmat <- stimmat[-min(stimmat$letternum[stimmat$line == n + 1]), ]
      
      # recompute letter number
      stimmat$letternum <- 1:nrow(stimmat)
      
      # recompute x positions
      stimmat$xs[stimmat$line == n + 1] <- c(x.offset, cumsum(stimmat$width[stimmat$line == 2]) + x.offset)[1:length(stimmat$width[stimmat$line == 2])]
      stimmat$xe[stimmat$line == n + 1] <- cumsum(stimmat$width[stimmat$line == 2]) + x.offset
    }
    
  }
  
  # Stage 2: compute wrap-up line breaks
  
  n <- 1; m <- 1
  while (n == m) {
    
    # set line break
    line.cut <- max(stimmat$wordnum[stimmat$line == n & stimmat$xe < x.cut])
    if (max(stimmat$xe[stimmat$line == n]) > x.cut) {
      
      # set new line
      stimmat$line[stimmat$line == n & stimmat$wordnum >= line.cut] <- stimmat$line[stimmat$line == n & stimmat$wordnum >= line.cut] + 1
      
      # delete blank before line break
      stimmat <- stimmat[-min(stimmat$letternum[stimmat$line == n + 1]), ]
      
      # recompute letter number
      stimmat$letternum <- 1:nrow(stimmat)
      
      # recompute x positions
      stimmat$xs[stimmat$line == n + 1] <- c(x.offset, cumsum(stimmat$width[stimmat$line == n + 1]) + x.offset)[1:length(stimmat$width[stimmat$line == n + 1])]
      stimmat$xe[stimmat$line == n + 1] <- cumsum(stimmat$width[stimmat$line == n + 1]) + x.offset
      
      # m <- m + 1
      
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
    
    stimmat$ys[stimmat$line == i] <- y.offset + font.lead + (font.height + 1) * (i - 1) + (font.height) * mult * (i - 1) - font.height * 0.5
    stimmat$ye[stimmat$line == i] <- y.offset + font.lead + (font.height + 1) * (i - 1) + (font.height) * mult * (i - 1) + font.height * 1.5
    
    # print (i)
  }
  
  
  # compute mean positions
  # -----------------------
  
  stimmat$xm <- (stimmat$xs + stimmat$xe) / 2
  stimmat$ym <- (stimmat$ys + stimmat$ye) / 2

  
  
  # reconstruct hyphens
  # --------------------
  
  if (length(hyph) > 0) {
    stimmat$letter[hyph] <- "-"
  }
  
    
  # determine target IA
  # --------------------
  
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    
    stimmat$target <- NA
    
    dat$trial[[trial]]$meta$target <- 
      grep(env$exp$setup$indicator$target, unlist(strsplit(stim, env$exp$setup$indicator$ia)))
    
    stimmat$target[stimmat$ianum == dat$trial[[trial]]$meta$target] <- "n"
    stimmat$target[stimmat$ianum == dat$trial[[trial]]$meta$target - 1] <- "n-1"
    stimmat$target[stimmat$ianum == dat$trial[[trial]]$meta$target + 1] <- "n+1"
    
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
  stimmat$letword <- ave(stimmat$letternum, stimmat$wordnum, FUN = rank) - 1
  
  first <- tapply(stimmat$word, stimmat$line, min)
  for (i in 1:length(first)) {
    stimmat$letword[stimmat$word == first[i]] <- stimmat$letword + 1
  }
  
  # letter in IA
  stimmat$letia <- ave(stimmat$letternum, stimmat$ianum, FUN = rank) - 1
  
  first <- tapply(stimmat$ia, stimmat$line, min)
  for (i in 1:length(first)) {
    stimmat$letia[stimmat$ia == first[i]] <- stimmat$letia + 1
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
  
  dat$trial[[trial]]$meta$stimmat <- stimmat
  
  return(dat)
  
}