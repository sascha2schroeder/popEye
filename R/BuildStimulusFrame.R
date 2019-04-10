
BuildStimulusFrame <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # TODO: line-wrap for hyphens
  
  # retrieve stimulus
  stim <- dat$trial[[trial]]$meta$stim
  
  # parse out ia delimiter and target indicator and split into letters
  tmp <- stim
  tmp <- gsub(env$exp$setup$indicator$target, "", tmp)
  tmp <- gsub(env$exp$setup$indicator$line, "", tmp)
  if (env$exp$setup$indicator$ia != " ") {
    tmp <- gsub(env$exp$setup$indicator$ia, "", tmp)
  }
  # FIX: replace hyphen by blank
  tmp <- gsub("-", " ", tmp)
  
  letters <- unlist(strsplit(tmp, ""))
  
  # variables
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
  
  # set up matrix
  stimmat <- data.frame(matrix(NA, length(letters), 10))
  colnames(stimmat) <- c("letter", "letno", "word", "ia", "line", "width",
                         "xs", "xe", "ys", "ye")
  
  
  # compute letter
  # ---------------
  
  stimmat$letter <- letters
  stimmat$letno <- 1:length(letters)

    
  # compute word
  # -------------
  
  word <- 1
  stimmat$width <- rep(NA, length = length(stimmat$letter))
  for (i in 1:length(letters)) {
    if (stimmat$letter[i] == " ") word <- word + 1
    stimmat$word[i] <- word
    stimmat$width[i] <- letpix$pixel[letpix$letter == stimmat$letter[i]]
    # print(i)
  }
  
  
  # compute IA
  # -----------
  
  if (env$exp$setup$indicator$ia == " ") {
    stimmat$ia <- stimmat$word
  } else {
    tmp <- stim
    tmp <- gsub(env$exp$setup$indicator$target, "", tmp)
    tmp <- gsub(env$exp$setup$indicator$line, "", tmp)
    ia.length <- sapply(unlist(strsplit(stim, ia.delim)), nchar)
    stimmat$ia <- rep(1:length(ia.length), each = ia.length)
  }
  # NOTE: check if this is correct
  
  
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
    
    # TODO: parse out target and IA indicator
    line.length <- sapply(unlist(strsplit(stim, line.delim)), nchar)
    
    nlines <- length(line.length)
    
    # line loop
    for (n in 1:(nlines - 1)) {
      
      if (line.length[n] == 0) next 
      
      stimmat$line[stimmat$line == n  & stimmat$letno >= line.length[n]] <- stimmat$line[stimmat$line == n & stimmat$letno >= line.length[n]] + 1
      
      # delete blank before line break
      stimmat <- stimmat[-min(stimmat$letno[stimmat$line == n + 1]), ]
      
      # recompute letter number
      stimmat$letno <- 1:nrow(stimmat)
      
      # recompute x positions
      stimmat$xs[stimmat$line == n + 1] <- c(x.offset, cumsum(stimmat$width[stimmat$line == 2]) + x.offset)[1:length(stimmat$width[stimmat$line == 2])]
      stimmat$xe[stimmat$line == n + 1] <- cumsum(stimmat$width[stimmat$line == 2]) + x.offset
    }
    
  }
  
  # Stage 2: compute wrap-up line breaks
  
  n <- 1; m <- 1
  while (n == m) {
    
    # set line break
    line.cut <- max(stimmat$word[stimmat$line == n & stimmat$xe < x.cut])
    if (max(stimmat$xe[stimmat$line == n]) > x.cut) {
      
      # set new line
      stimmat$line[stimmat$line == n & stimmat$word >= line.cut] <- stimmat$line[stimmat$line == n & stimmat$word >= line.cut] + 1
      
      # delete blank before line break
      stimmat <- stimmat[-min(stimmat$letno[stimmat$line == n + 1]), ]
      
      # recompute letter number
      stimmat$letno <- 1:nrow(stimmat)
      
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
  
  # for (i in 1:max(stimmat$line)) {
  #   # i = 1
  #   stimmat$ys[stimmat$line == i] <- y.offset + font.height*((i - 1) * (1 + mult) - 1) + font.lead*(2 * (i - 1))
  #   stimmat$ye[stimmat$line == i] <- y.offset + font.height*(i * (1 + mult) - 1) + font.lead*(2 * i)
  #   # print (i)
  # }
  
  for (i in 1:max(stimmat$line)) {
    # i = 1
    
    # stimmat$ys[stimmat$line == i] <- y.offset + font.lead + (font.height + 1) * (i - 1) + (font.height - 1) * mult * (i - 1) - font.height * 0.5
    # stimmat$ye[stimmat$line == i] <- y.offset + font.lead + (font.height + 1) * (i - 1) + (font.height - 1) * mult * (i - 1) + font.height * 1.5
    
    stimmat$ys[stimmat$line == i] <- y.offset + font.lead + (font.height + 1) * (i - 1) + (font.height) * mult * (i - 1) - font.height * 0.5
    stimmat$ye[stimmat$line == i] <- y.offset + font.lead + (font.height + 1) * (i - 1) + (font.height) * mult * (i - 1) + font.height * 1.5
    
    # print (i)
  }
  
  
  # compute mean positions
  # -----------------------
  
  stimmat$xm <- (stimmat$xs + stimmat$xe) / 2
  stimmat$ym <- (stimmat$ys + stimmat$ye) / 2
  # stimmat$ym <- stimmat$ys + font.height/2
  
  # determine target IA
  # --------------------
  
  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    dat$trial[[trial]]$meta$target <- 
      grep(env$exp$setup$indicator$target, unlist(strsplit(stim, env$exp$setup$indicator$ia)))
  }
  # TODO: save in stimmat?
  
  
  # letter positions
  # ----------------
  
  # letter in line
  stimmat$letline <- ave(stimmat$letno, stimmat$line, FUN = rank)
  
  # letter in word
  stimmat$letword <- ave(stimmat$letno, stimmat$word, FUN = rank) - 1
  
  first <- tapply(stimmat$word, stimmat$line, min)
  for (i in 1:length(first)) {
    stimmat$letword[stimmat$word == first[i]] <- stimmat$letword + 1
  }
  
  # letter in IA
  stimmat$letia <- ave(stimmat$letno, stimmat$ia, FUN = rank) - 1
  
  first <- tapply(stimmat$ia, stimmat$line, min)
  for (i in 1:length(first)) {
    stimmat$letia[stimmat$ia == first[i]] <- stimmat$letia + 1
  }
  
  
  # return
  # -------
  
  dat$trial[[trial]]$meta$stimmat <- stimmat
  
  return(dat)
  
}