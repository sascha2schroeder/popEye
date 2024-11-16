
ReadStimulus2 <- function(dat, env = parent.frame(n = 1)) {
  
  stimfile <- env$exp$setup$stimulus$file
  
  # compute match variable (if not provided)
  if (sum(is.na(env$exp$setup$stimulus$cond) == TRUE) > 0) {
    
    # create match variable (itemid)
    stimfile$cond <- 1
    stimfile$match <- paste(stimfile[, match(env$exp$setup$stimulus$id, colnames(stimfile))], 1, sep = ":")
    
  } else {
    
    if (length(env$exp$setup$stimulus$cond) == 1) {
      
      # create match variable (itemid:cond)
      stimfile$match <- paste(stimfile[, match(env$exp$setup$stimulus$id, colnames(stimfile))],
                              stimfile[, match(env$exp$setup$stimulus$cond, colnames(stimfile))], sep = ":")
       
    } else if (length(env$exp$setup$stimulus$cond) == 2) {
      
      # create combined condition variable
      cond1 <- env$exp$setup$stimulus$cond[1]
      cond2 <- env$exp$setup$stimulus$cond[2]
      
      stimfile$cond <- paste(stimfile[, match(cond1, colnames(stimfile))],
                             stimfile[, match(cond2, colnames(stimfile))], sep = ":")
      
      # create match variable (itemid:cond)
      stimfile$match <- paste(stimfile[, match(env$exp$setup$stimulus$id, colnames(stimfile))],
                              stimfile$cond, sep = ":")
      
    }
    
  }
  
  
  # parse out indicators
  
  # parse out indicator characters from text display
  stimfile$stim <- stimfile[, match(env$exp$setup$stimulus$text, colnames(stimfile))]
  stimfile$text <- gsub(env$exp$setup$indicator$target, "", stimfile$stim)
  if (env$exp$setup$indicator$word != "") {
    stimfile$text <- gsub(env$exp$setup$indicator$word, " ", stimfile$text)
  }
  stimfile$text <- gsub(env$exp$setup$indicator$line, " ", stimfile$text)
  if (env$exp$setup$indicator$ia != " ") {
    stimfile$text <- gsub(env$exp$setup$indicator$ia, "", stimfile$text)
  }
  
  # parse out indicator characters from preview display
  if (env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    stimfile$preview <- stimfile[, match(env$exp$setup$stimulus$preview, colnames(stimfile))]
    stimfile$preview <- gsub(env$exp$setup$indicator$target, "", stimfile$preview)
    if (env$exp$setup$indicator$word != "") {
      stimfile$preview <- gsub(env$exp$setup$indicator$word, " ", stimfile$preview)
    }
    stimfile$preview <- gsub(env$exp$setup$indicator$line, " ", stimfile$preview)
    if (env$exp$setup$indicator$ia != " "){
      stimfile$preview <- gsub(env$exp$setup$indicator$ia, "", stimfile$preview)
    }
  }
  
  # parse out indicator characters from prime display
  if (env$exp$setup$type == "fast") {
    stimfile$prime <- stimfile[, match(env$exp$setup$stimulus$prime, colnames(stimfile))]
    stimfile$prime <- gsub(env$exp$setup$indicator$target, "", stimfile$prime)
    if (env$exp$setup$indicator$word != "") {
      stimfile$prime <- gsub(env$exp$setup$indicator$word, " ", stimfile$prime)
    }
    stimfile$prime <- gsub(env$exp$setup$indicator$line, " ", stimfile$prime)
    if (env$exp$setup$indicator$ia != " "){
      stimfile$prime <- gsub(env$exp$setup$indicator$ia, "", stimfile$prime)
    }
  }
  
  
  # compute stimmat
  # ---------------
  
  x.offset <- env$exp$setup$display$marginLeft
  y.offset <- env$exp$setup$display$marginTop
  line.delim <- env$exp$setup$indicator$line
  letpix <- env$exp$setup$font$letpix
  font.height <- env$exp$setup$font$height
  font.lead <- env$exp$setup$font$lead
  x.cut <- env$exp$setup$display$resolutionX - env$exp$setup$display$marginRight
  
  env$exp$setup$stimulus$stimmat <- list()
  
  for (s in 1:nrow(stimfile)) {
    # s <- 2
    
    stim <- trimws(stimfile$stim[s], which = "both")
  
    # compute code points
    
    # split into Unicode code points and parse out ia delimiter and target indicator
    tmp_points <- stim
    tmp_points <- gsub(env$exp$setup$indicator$target, "", tmp_points)
    tmp_points <- gsub(env$exp$setup$indicator$line, "", tmp_points)
    
    # TODO: this is only EB behavior; maybe condition on software
    # replace hyphen within word with different character
    tmp_points <- unlist(strsplit(tmp_points, " "))
    sel <- grep(".-.", tmp_points)
    tmp_points[sel] <- gsub("[-]", "\u20de", tmp_points[sel])
    tmp_points <- paste(tmp_points, collapse = " ")
    
    # compute code points
    points <- unlist(strsplit(tmp_points, ""))
    
    # segment words
    tmp_word <- tmp_points
    if (env$exp$setup$stimulus$hyphenwrap == T) {
      tmp_word <- gsub("\u20de", "\u20de ", tmp_word)
    }
    tmp_word <- gsub("\u20de", "-", tmp_word)
    
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
    
    # compute pointmat
    
    pointmat <- data.frame(matrix(NA, length(points), 1))
    colnames(pointmat) <- c("match")
    # TODO: assign screen, assign text
    # TODO: only one letter loop?
    
    # structural variables
    
    pointmat$match <- stimfile$match[s]
    pointmat$itemid <- stimfile[s, match(env$exp$setup$stimulus$id, colnames(stimfile))]
    pointmat$cond <- stimfile$cond[s]
    
    # point, letter, glyph
    pointmat$pointnum <- 1:length(points)
    pointmat$point <- points
    pointmat$pointwidth <- NA
    pointmat$letternum <- NA
    pointmat$letter <- NA
    # pointmat$letterwidth <- NA
    pointmat$glyphnum <- NA
    pointmat$glyph <- NA
    # pointmat$glyphwidth <- NA
    
    # words
    # TODO: punctuation part of word?
    if (env$exp$setup$indicator$word != "") {
      words <- unlist(strsplit(tmp_word, env$exp$setup$indicator$word))
    } else {
      words <- unlist(strsplit(tmp_word, env$exp$setup$separator$word))
    }
    
    wordnum <- 1
    pointmat$wordnum <- NA
    pointmat$word <- NA
    
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
    pointmat$sentnum <- NA
    pointmat$sent <- NA
    sentmem <- FALSE
    
    # IA
    if (env$exp$setup$indicator$ia != "") {
      
      tmp_ia <- stim
      tmp_ia <- gsub(env$exp$setup$indicator$target, "", tmp_ia)
      tmp_ia <- gsub(env$exp$setup$indicator$line, "", tmp_ia)
      tmp_ia <- gsub(env$exp$setup$indicator$word, "", tmp_ia)
      
      ias <- unlist(strsplit(tmp_ia, env$exp$setup$indicator$ia))
      ianum <- 1
      pointmat$ianum <- NA
      pointmat$ia <- NA
      mem <- nchar(ias[ianum])
      
    } 
    
    
    # code point loop
    # ----------------
    
    lig <- F
    letternum <- 0
    glyphnum <- 0
    
    i <- 1
    while (i <= nrow(pointmat)) {
    # while (i <= 40) {
      
      # words
      
      # check indicator
      if (is.element(pointmat$point[i], env$exp$setup$indicator$word)) {
        wordnum <- wordnum + 1
        pointmat <- pointmat[-i, ]
        # i <- i - 1
        next
      }
      
      # check separator
      if (is.element(pointmat$point[i], env$exp$setup$separator$word)) wordnum <- wordnum + 1
      # TODO: Check hyphens
      if (env$exp$setup$stimulus$hyphenwrap == T) {
        if (i > 1) {
          if (pointmat$point[i - 1] == "\u20de") wordnum <- wordnum + 1
        }
      }
      
      pointmat$wordnum[i] <- wordnum
      pointmat$word[i] <- words[wordnum]
      
      # sentence
      
      # if(is.element(pointmat$point[i], env$exp$setup$separator$sentence)) {
      #   sentmem <- TRUE
      # }
      
      # check sentence2 separator
      if (is.element(pointmat$point[i], env$exp$setup$separator$sentence2) & sentmem == TRUE) {
        sentnum <- sentnum + 1
        sentmem <- FALSE
      } else if (is.element(pointmat$point[i], env$exp$setup$separator$sentence) == TRUE & sentmem == FALSE) {
        sentmem <- TRUE
      } else if (is.element(pointmat$point[i], env$exp$setup$separator$sentence2) == FALSE & sentmem == TRUE) {
        sentmem <- FALSE
      }
    
      pointmat$sentnum[i] <- sentnum
      sent.let <- unlist(strsplit(sent[sentnum], ""))
      sent.n <- length(sent.let)
      if (sent.n > 20) sent.n <- 20
      pointmat$sent[i] <- paste(sent.let[1:sent.n], collapse = "")
      pointmat$sent.nwords[i] <- sent.nwords[sentnum]
      pointmat$sent.nletters[i] <- sent.nletters[sentnum]
      # TODO: maybe rename letter -> code
      
      # IA
      if (env$exp$setup$indicator$ia == "") {
        
        pointmat$ianum[i] <- pointmat$wordnum[i]
        pointmat$ia[i] <- pointmat$word[i]
        
      } else {
        
        if (pointmat$pointnum[i] > mem) {
          ianum <- ianum + 1
          mem <- mem + nchar(ias[ianum])
        }
        
        pointmat$ianum[i] <- ianum
        ia.let <- unlist(strsplit(ias[ianum], ""))
        ia.n <- length(ia.let)
        if (ia.n > 20) ia.n <- 20
        pointmat$ia[i] <- paste(ia.let[1:ia.n], collapse = "")
        # TODO: maybe rename letter -> code
        
      }
      
      # compute code point width
      if (env$exp$setup$font$fixed == FALSE) {
        
        if (is.element(pointmat$point[i], letpix$letter) == F) {
          print(paste("Code point", pointmat$point[i], "missing.", sep = " "))
        }
        
        if (i < nrow(pointmat)) {
          
          if (pointmat$point[i] == "\u094d") {
            # TODO: set as parameter in popEye 
            
            if (is.element(pointmat$point[i - 1], env$exp$setup$font$dia) == F) {
              
              pointmat$pointwidth[i] <- letpix$pixel[letpix$letter == pointmat$point[i]]
              
              pointmat$pointwidth[i - 1] <- env$exp$setup$font$lig_letpix$pixel[env$exp$setup$font$lig_letpix$letter == pointmat$point[i - 1]]
              
              # irregular halants
              if (pointmat$point[i - 1] == "\u0915" & pointmat$point[i + 1] == "\u0924") pointmat$pointwidth[i - 1] <- 7
              if (pointmat$point[i - 1] == "\u0915" & pointmat$point[i + 1] == "\u0937") pointmat$pointwidth[i - 1] <- 5
              if (pointmat$point[i - 1] == "\u0915" & pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i - 1] <- 11
              if (pointmat$point[i - 1] == "\u0916" & pointmat$point[i + 1] == "\u092f") pointmat$pointwidth[i - 1] <- 17
              if (pointmat$point[i - 1] == "\u0916" & pointmat$point[i + 1] == "\u0924") pointmat$pointwidth[i - 1] <- 17 
              if (pointmat$point[i - 1] == "\u0917" & pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i - 1] <- 6
              if (pointmat$point[i - 1] == "\u091c" & pointmat$point[i + 1] == "\u091e") pointmat$pointwidth[i - 1] <- -3 
              if (pointmat$point[i - 1] == "\u091f" & pointmat$point[i + 1] == "\u0920") pointmat$pointwidth[i - 1] <- -2
              if (pointmat$point[i - 1] == "\u091f" & pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i - 1] <- 4
              if (pointmat$point[i - 1] == "\u0921" & pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i - 1] <- 6
              if (pointmat$point[i - 1] == "\u0924" & pointmat$point[i + 1] == "\u0924") pointmat$pointwidth[i - 1] <- 2
              if (pointmat$point[i - 1] == "\u0924" & pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i - 1] <- 4
              if (pointmat$point[i - 1] == "\u0926" & pointmat$point[i + 1] == "\u0926") pointmat$pointwidth[i - 1] <- -1
              if (pointmat$point[i - 1] == "\u0926" & pointmat$point[i + 1] == "\u0927") pointmat$pointwidth[i - 1] <- 2
              if (pointmat$point[i - 1] == "\u0926" & pointmat$point[i + 1] == "\u092c") pointmat$pointwidth[i - 1] <- -2
              if (pointmat$point[i - 1] == "\u0926" & pointmat$point[i + 1] == "\u092e") pointmat$pointwidth[i - 1] <- 2
              if (pointmat$point[i - 1] == "\u0926" & pointmat$point[i + 1] == "\u092f") pointmat$pointwidth[i - 1] <- 2
              if (pointmat$point[i - 1] == "\u0926" & pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i - 1] <- 4
              if (pointmat$point[i - 1] == "\u0926" & pointmat$point[i + 1] == "\u0935") pointmat$pointwidth[i - 1] <- -2
              if (pointmat$point[i - 1] == "\u092a" & pointmat$point[i + 1] == "\u0924") pointmat$pointwidth[i - 1] <- 0
              if (pointmat$point[i - 1] == "\u092a" & pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i - 1] <- 5
              if (pointmat$point[i - 1] == "\u092b" & pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i - 1] <- 10
              if (pointmat$point[i - 1] == "\u0936" & pointmat$point[i + 1] == "\u091a") pointmat$pointwidth[i - 1] <- 0
              if (pointmat$point[i - 1] == "\u0936" & pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i - 1] <- 8
              if (pointmat$point[i - 1] == "\u0936" & pointmat$point[i + 1] == "\u0932") pointmat$pointwidth[i - 1] <- 6
              if (pointmat$point[i - 1] == "\u0936" & pointmat$point[i + 1] == "\u0935") pointmat$pointwidth[i - 1] <- 2
              if (pointmat$point[i - 1] == "\u0937" & pointmat$point[i + 1] == "\u091f") pointmat$pointwidth[i - 1] <- 1
              if (pointmat$point[i - 1] == "\u0937" & pointmat$point[i + 1] == "\u0920") pointmat$pointwidth[i - 1] <- 0
              if (pointmat$point[i - 1] == "\u0939" & pointmat$point[i + 1] == "\u092e") pointmat$pointwidth[i - 1] <- 3
              if (pointmat$point[i - 1] == "\u0939" & pointmat$point[i + 1] == "\u092f") pointmat$pointwidth[i - 1] <- 5
              
              # halant chains
              if (i > 3) {

                if (pointmat$point[i - 3] == "\u0937" & 
                    pointmat$point[i - 2] == "\u094d" & 
                    pointmat$point[i - 1] == "\u091f" & 
                    pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i] <- 8
                
                if (pointmat$point[i - 3] == "\u0938" & 
                    pointmat$point[i - 2] == "\u094d" & 
                    pointmat$point[i - 1] == "\u0924" & 
                    pointmat$point[i + 1] == "\u0930") pointmat$pointwidth[i] <- -5
                
              }
              
            } else {
              
              # if diacritic before halant, revise consonant before
              pointmat$pointwidth[i - 2] <- env$exp$setup$font$lig_letpix$pixel[env$exp$setup$font$lig_letpix$letter == pointmat$point[i - 2]]
              # pointmat$letterwidth[i - 2] <- env$exp$setup$font$lig_letpix$pixel[env$exp$setup$font$lig_letpix$letter == pointmat$point[i - 2]]
              # pointmat$glyphwidth[i - 2] <- env$exp$setup$font$lig_letpix$pixel[env$exp$setup$font$lig_letpix$letter == pointmat$point[i - 2]]
              
              pointmat$pointwidth[i] <- letpix$pixel[letpix$letter == pointmat$point[i]]
              
            }
            
          } else if (pointmat$point[i] == "\u0942") {

            pointmat$pointwidth[i] <- letpix$pixel[letpix$letter == pointmat$point[i]]

            if (pointmat$point[i - 1] == "\u0930") pointmat$pointwidth[i - 1] <- 19
            
          } else if (pointmat$point[i] == "\u0941") {
            
            pointmat$pointwidth[i] <- letpix$pixel[letpix$letter == pointmat$point[i]]
            
            if (pointmat$point[i - 1] == "\u0930") pointmat$pointwidth[i - 1] <- 13
            
          } else {
            
            pointmat$pointwidth[i] <- letpix$pixel[letpix$letter == pointmat$point[i]]
            
          }
          
        } else {
          
          pointmat$pointwidth[i] <- letpix$pixel[letpix$letter == pointmat$point[i]]
          
        } 
        
      } else {
        
        if (is.element(pointmat$point[i], env$exp$setup$font$half) == T) {
          weight <- .5
        } else {
          weight <- 1
        }
        
        pointmat$pointwidth[i] <- letpix$pixel[1]*weight
        
      }
      
      # compute letter 
      
      letternum <- letternum + 1
      glyphnum <- glyphnum + 1
      
      if (is.element(pointmat$point[i], env$exp$setup$font$dia)) {
        
        letternum <- letternum - 1
        pointmat$letternum[i] <- letternum
        pointmat$letter[i] <- paste(pointmat$letter[i - 1], pointmat$point[i], collapse = "", sep = "")
        # pointmat$letterwidth[i] <- pointmat$letterwidth[i - 1] + pointmat$pointwidth[i]
        
        glyphnum <- glyphnum - 1
        pointmat$glyphnum[i] <- glyphnum
        pointmat$glyph[i] <- paste(pointmat$glyph[i - 1], pointmat$point[i], collapse = "", sep = "")
        # pointmat$glyphwidth[i] <- pointmat$glyphwidth[i - 1] + pointmat$pointwidth[i]
        
        if (pointmat$point[i] == "\u094d") {
          lig <- T
        }
        
      } else {
        
        if (lig == F) {
          
          pointmat$letternum[i] <- letternum
          pointmat$letter[i] <- pointmat$point[i]
          # pointmat$letterwidth[i] <- pointmat$pointwidth[i]
          
          pointmat$glyphnum[i] <- glyphnum
          pointmat$glyph[i] <- pointmat$point[i]
          # pointmat$glyphwidth[i] <- pointmat$pointwidth[i]
          
          
        } else {
          
          pointmat$letternum[i] <- letternum
          pointmat$letter[i] <- pointmat$point[i]
          # pointmat$letterwidth[i] <- pointmat$pointwidth[i - 1]
          
          glyphnum <- glyphnum - 1
          pointmat$glyphnum[i] <- glyphnum
          pointmat$glyph[i] <- paste(pointmat$glyph[i - 1], pointmat$point[i], collapse = "", sep = "")
          # pointmat$glyphwidth[i] <- pointmat$glyphwidth[i - 1] + pointmat$pointwidth[i]
          
          lig <- F
          
        }
        
      }
      
      i <- i + 1
      
    }
    
    pointmat$pointnum <- 1:nrow(pointmat)
    row.names(pointmat) <- NULL
    
    
    # # -----------------------------------------------------
    # 
    # # aggregate on letter level
    # letmat <- aggregate(. ~ letternum, data = stimmat, FUN = tail, 1)
    # 
    # # number code points
    # npoints <- aggregate(stimmat$pointnum, list(stimmat$letternum), length)
    # colnames(npoints) <- c("letternum", "npoints")
    # 
    # letmat2 <- merge(letmat, npoints)
    # letmat2$pointnum <- NULL
    # letmat2$point <- NULL
    # letmat2$pointwidth <- NULL
    # letmat2$glyphnum <- NULL
    # letmat2$glyph <- NULL
    # letmat2$glyphwidth <- NULL
    # 
    # 
    # # compute initial x positions
    # # ----------------------------
    # letmat2$xs <- c(x.offset, cumsum(letmat2$letterwidth) + x.offset)[1:length(letmat2$letterwidth)]
    # letmat2$xe <- cumsum(letmat2$letterwidth) + x.offset
    # # NOTE: separate start and end positions necessary?
    # 
    # stimmat <- letmat2
    # stimmat <- stimmat[order(stimmat$letternum, stimmat$wordnum), ]
    # stimmat$itemid <- as.numeric(stimmat$itemid)
    # stimmat$width <- as.numeric(stimmat$letterwidth)
    # stimmat$wordnum <- as.numeric(stimmat$wordnum)
    # stimmat$sentnum <- as.numeric(stimmat$sentnum)
    # stimmat$sent.nwords <- as.numeric(stimmat$sent.nwords)
    # stimmat$sent.nletters <- as.numeric(stimmat$sent.nletters)
    # stimmat$ianum <- as.numeric(stimmat$ianum)
    # 
    # # -----------------------------------------------------
    
    # -----------------------------------------------------
    
    
    # glyphmat <- pointmat[c(1, diff(pointmat$glyphnum)) == 1, ]
    
    glyphmat <- pointmat[duplicated(pointmat$glyphnum, fromLast = T) == F, ]
    
    # number code points
    glypthwidth <- aggregate(pointmat$pointwidth, list(pointmat$glyphnum), sum)
    colnames(glypthwidth) <- c("glyphnum", "glyphwidth")
    glyphpoints <- aggregate(pointmat$pointnum, list(pointmat$glyphnum), length)
    colnames(glyphpoints) <- c("glyphnum", "npoints")
    
    glyphmat2 <- merge(merge(glyphmat, glypthwidth, by = "glyphnum"), glyphpoints, by = "glyphnum")
    glyphmat2$pointnum <- NULL
    glyphmat2$point <- NULL
    glyphmat2$pointwidth <- NULL
    glyphmat2$letternum <- NULL
    glyphmat2$letter <- NULL
    glyphmat2$letterwidth <- NULL
    
    
    # compute initial x positions
    # ----------------------------
    glyphmat2$xs <- c(x.offset, cumsum(glyphmat2$glyphwidth) + x.offset)[1:length(glyphmat2$glyphwidth)]
    glyphmat2$xe <- cumsum(glyphmat2$glyphwidth) + x.offset
    # NOTE: separate start and end positions necessary?
    
    stimmat <- glyphmat2
    stimmat$letternum <- stimmat$glyphnum
    stimmat$glyphnum <- NULL
    stimmat$letter <- stimmat$glyph
    stimmat$letterwdith <- stimmat$glyphwidth
    stimmat$glyph <- NULL
    stimmat <- stimmat[order(stimmat$letternum, stimmat$wordnum), ]
    stimmat$itemid <- as.numeric(stimmat$itemid)
    stimmat$width <- as.numeric(stimmat$glyphwidth)
    stimmat$glyphwidth <- NULL
    stimmat$wordnum <- as.numeric(stimmat$wordnum)
    stimmat$sentnum <- as.numeric(stimmat$sentnum)
    stimmat$sent.nwords <- as.numeric(stimmat$sent.nwords)
    stimmat$sent.nletters <- as.numeric(stimmat$sent.nletters)
    stimmat$ianum <- as.numeric(stimmat$ianum)
    
    
    # -----------------------------------------------------
    
    
    # compute lines
    # --------------
    
    # NOTE: maybe condition on software (ET vs EB)
    
    stimmat$line <- 1
    
    # Stage 1: compute manual line breaks
    
    if (grepl(line.delim, stim) == T) {
      
      # TODO: move up?
      tmp_line <- stim
      tmp_line <- gsub(env$exp$setup$indicator$target, "", tmp_line)
      tmp_line <- gsub(env$exp$setup$indicator$word, "", tmp_line)
      if (env$exp$setup$indicator$ia != " ") {
        tmp_line <- gsub(env$exp$setup$indicator$ia, "", tmp_line)
      }
      
      line.length <- sapply(unlist(strsplit(tmp_line, line.delim)), nchar)
      line.length2 <- pointmat$glyphnum[line.length]
      
      nlines <- length(line.length)
      
      # line loop
      for (n in 1:(nlines - 1)) {
        # n <- 1
        
        if (line.length2[n] == 0) next 
        
        stimmat$line[stimmat$line == n  & stimmat$letternum > cumsum(line.length2)[n]] <- n + 1
        
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
      
      if (env$exp$setup$font$wrap == T & length(stimmat$wordnum[stimmat$line == n]) > 0) {
        
        # set line break
        line.cut <- max(stimmat$wordnum[stimmat$line == n & stimmat$xe <= x.cut])
        
        if (env$exp$setup$stimulus$hyphenwrap == T) {
          if (stimmat$letter[max(stimmat$letternum[stimmat$wordnum == line.cut])] == "\u20de") {
            line.cut <- line.cut + 1
          }
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
    
    
    # TODO: Extract target position during trial assignment
    # determine target IA
    # --------------------

    if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {

      stimmat$target <- NA
      if (env$exp$setup$indicator$ia != "") {
        target <- grep(env$exp$setup$indicator$target, unlist(strsplit(stim, env$exp$setup$indicator$ia)))
      } else {
        target <- grep(env$exp$setup$indicator$target, unlist(strsplit(stim, env$exp$setup$separator$word)))
      }

      stimmat$target[stimmat$ianum == target] <- "n"
      stimmat$target[stimmat$ianum == target - 1] <- "n-1"
      stimmat$target[stimmat$ianum == target + 1] <- "n+1"

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
      if (is.element(stimmat$letter[stimmat$ianum == i & stimmat$letia == 1], env$exp$setup$separator$ia)) {
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
      
      names <- c("match", "itemid", "cond", "trial", "trial.nwords",
                 "letternum", "letter", "wordnum", "word", "sentnum", "sent", "sent.nwords",
                 "ianum", "ia", "target", "width", "line", 
                 "xs", "xe", "ys", "ye", "xm", "ym",
                 "letline", "letword", "letia", "wordline", "wordsent")
    } else {
      
      names <- c("match", "itemid", "cond", "trial", "trial.nwords",
                 "letternum", "letter", "wordnum", "word", "sentnum", "sent", "sent.nwords",
                 "ianum", "ia", "width", "line", 
                 "xs", "xe", "ys", "ye", "xm", "ym",
                 "letline", "letword", "letia", "wordline", "wordsent")
    }
    
    env$exp$setup$stimulus$pointmat[[s]] <- pointmat
    env$exp$setup$stimulus$stimmat[[s]] <- stimmat[names]
    names(env$exp$setup$stimulus$stimmat)[s] <- stimmat$match[1]
    
  }
  
  return(dat)
  
}