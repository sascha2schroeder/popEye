
SetupExperiment <- function(env = parent.frame(n = 1)) {
  
  
  # setup tracker
  # --------------
  
  if (env$tracker.model == "") env$tracker.model <- "eyelink"
  if (env$tracker.software == "") env$tracker.software <- "EB"
  if (env$tracker.results == "") env$tracker.results <- T
  tracker <- list(model = env$tracker.model, software = env$tracker.software,
                  results = env$tracker.results)
  
  
  # setup experiment type
  # ----------------------
  
  if (env$type == "") env$type <- "sentence"
  type <- env$type
  
  
  # messages
  # ---------
  
  message <- list(start = NA, stop = NA)  
  message$start <- env$message.start
  message$stop <- env$message.stop
  
  if (env$type == "boundary" | env$type == "fast") {
    message$boundary <- env$message.boundary
    message$target <- env$message.target
  }

  if (env$type == "fast") {
    message$prime <- env$message.prime
  }
  
  
  # variable
  # ---------
  if (env$variable.id == "") env$variable.id <- "id"
  variable.id <- env$variable.id
  if (env$variable.cond == "") env$variable.cond <- NA
  # if (length(env$variable.cond) > 1) env$variable.cond <- paste(env$variable.cond, collapse = ":")
  variable.cond <- env$variable.cond

  variable <- list(id = env$variable.id,
               cond = env$variable.cond)
  
  
  # item
  # -----
  
  if (env$item.pracnum == "") env$item.pracnum <- 0
  if (env$item.practice == "") env$item.practice <- "^P"
  if (env$item.trigger == "") env$item.trigger <- "999"
  if (env$item.question == "") env$item.question <- 1000
  if (length(env$item.keep) == 1) env$item.keep <- ""
  
  item <- list(pracnum = env$item.pracnum,
               practice = env$item.practice,
               trigger = env$item.trigger,
               question = env$item.question,
               keep = env$item.keep)
  
  
  # stimfile
  # ---------
 
  if (env$stimulus.id == "") env$stimulus.id <- "id"
  if (env$stimulus.cond == "") env$stimulus.cond <- NA
  # if (length(env$stimulus.cond) > 1) env$stimulus.cond <- paste(env$stimulus.cond, collapse = ":")
  if (env$stimulus.preview == "") env$stimulus.preview <- "preview"
  if (env$stimulus.prime == "") env$stimulus.prime <- "prime"
  if (env$stimulus.text == "") env$stimulus.text <- "text"
  
  stimulus <- list(file = env$stimulus.file,
                   id = env$stimulus.id,
                   cond = env$stimulus.cond,
                   preview = env$stimulus.preview,
                   prime = env$stimulus.prime,
                   text = env$stimulus.text)

  
  # indicator
  # ----------
    
  if (env$indicator.word == "") env$indicator.word <- " "
  if (env$indicator.ia == "") env$indicator.ia <- " "
  if (env$indicator.target == "") env$indicator.target <- "\\*"
  if (env$indicator.line == "") env$indicator.line <- "\\\\n"
  
  indicator <- list(word = env$indicator.word,
                    ia = env$indicator.ia,
                    target = env$indicator.target,
                    line = env$indicator.line)                 
  
  
  # display
  # --------
  
  if (env$display.marginLeft == "") env$display.marginLeft <- 150
  if (env$display.marginTop == "") env$display.marginTop <- 300
  if (env$display.marginRight == "") env$display.marginRight <- 50
  if (env$display.marginBottom == "") env$display.marginBottom <- 100
  display <- list(marginLeft = env$display.marginLeft,
                  marginTop = env$display.marginTop,
                  marginRight = env$display.marginRight,
                  marginBottom = env$display.marginBottom)
  # TODO: add physical width/height/dist in cm (for visual angle calculations)
  # TODO: add aspect ration(4:3, 16:10, 16:9 for plots)
  
  
  # font
  # -----
  
  # TODO: specify height indicators for all fonts
  
  # font type
  if (env$font.name == "") env$font.name <- "CourierNew"
  if (env$font.size == "") env$font.size <- 16
  if (env$font.spacing == "") env$font.spacing <- "2"
  font <- list(name = env$font.name, 
               size = env$font.size,
               spacing = env$font.spacing)
  
  if (env$font.name == "CourierNew") {
    font$family <- "mono"
  } else if (env$font.name == "Symbol") {
    font$family <- "HersheySymbol"
  } else {
    font$family <- "unknown"
  }
  
  # CourierNew, 14 pt
  if (font$name == "CourierNew" & font$size == 14) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!","–", "-","’","´",
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
    pixel <- rep(11, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 16
    font$lead <- 3
  }
  
  # CourierNew, 16 pt
  if (font$name == "CourierNew" & font$size == 16) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!","–", "-","’","´",
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
    pixel <- rep(13, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 18
    font$lead <- 4
    
  }
  
  # CourierNew, 18 pt
  if (font$name == "CourierNew" & font$size == 18) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!","–", "-","’","´", "%",":","‘","'","’"," ",
                "ﬀ","ﬁ",
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
    pixel <- rep(14, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 20
    font$lead <- 5
    
  }
  
  # CourierNew, 20 pt
  if (font$name == "CourierNew" & font$size == 20) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!","–", "-","’","´",
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
    pixel <- rep(16, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 22
    font$lead <- 6
    
  }
  
  # Consolas, 20 pt
  if (font$name == "Consolas" & font$size == 20) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!","–", "-","’","´","„","“",":","\"",";","”",
                "(", ")","'",
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
    pixel <- rep(15, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 24
    font$lead <- 10
    
  }
  
  # Symbol, 13 pt
  if (font$name == "Symbol" & font$size == 13) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!")
    pixel <- c(19,17,17,19,17,17,19,16,19,8,11,21,20,22,19,19,13,
               19,18,15,16,18,19,NA,13,20,NA,21,17,
               14,14,13,14,12,11,15,13,14,8,11,14,14,13,13,14,13,15,12,13,16,12,11,
               13,13,17,17,13,17,10,
               12,6,5,NA,NA)
    # NOTE: "Ö" correct?
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    
  }

  # Symbol, 14 pt
  if (font$name == "Symbol" & font$size == 14) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!")
    pixel <- c(21,17,16,21,19,18,19,18,20,9,12,21,21,22,18,20,NA,
               20,NA,15,16,19,21,NA,12,22,NA,NA,18,
               15,15,13,14,12,11,15,13,14,7,11,14,16,15,13,13,13,
               16,NA,13,16,14,11,13,13,17,17,13,NA,11,
               14,3,3,NA,NA)
    # NOTE: "I" correct?
    font$letpix <- data.frame(letter = letter, pixel = pixel) 
  }
  
  # print classes
  font$print$up <- c("A","E","I","O","U","Q","W","R","T","Z","P","S","D","F",
                     "G","H","J","K","L","Y","X","C","V","B","N","M","Ä","Ö",
                     "Ü","d","f","h","k","l","t","b","ß","i","ä","ö","ü","?",
                     "!")
  font$print$mi <- c("v","w","r","z","s","x","c","n","m","a","e","o","u")
  font$print$de <- c("q","p","g","j","y")
  font$print$pu <- c(".",",","–")

  # TODO: multiline experiments: line spacing matrix (separat or in ymargin?)
  
  
  # analysis
  # ---------
  if (env$analysis.eyelink == "") env$analysis.eyelink <- FALSE
  if (env$analysis.vfac == "") env$analysis.vfac <- 5
  if (env$analysis.mindur == "") env$analysis.mindur <- 10
  if (env$analysis.postdur == "") env$analysis.postdur <- 30
  if (env$analysis.drift == "") env$analysis.drift <- TRUE
  if (env$analysis.sparse == "") env$analysis.sparse <- TRUE
  if (env$analysis.driftX == "") env$analysis.driftX <- FALSE
  if (env$analysis.driftY == "") env$analysis.driftY <- FALSE
  if (env$analysis.lineMethod == "") env$analysis.lineMethod <- "match"
  if (env$analysis.outlierX == "") env$analysis.outlierX <- 2
  if (env$analysis.outlierY == "") env$analysis.outlierY <- 2
  if (env$analysis.lineX == "") env$analysis.lineX <- 2
  if (env$analysis.lineY == "") env$analysis.lineY <- 2
  
  analysis <- list(eyelink = env$analysis.eyelink, vfac = env$analysis.vfac,
                   mindur = env$analysis.mindur, postdur = env$analysis.postdur,
                   drift = env$analysis.drift, sparse = env$analysis.sparse,
                   driftX = env$analysis.driftX, driftY = env$analysis.driftY,
                   lineMethod = env$analysis.lineMethod,
                   outlierX = env$analysis.outlierX,
                   outlierY = env$analysis.outlierY,
                   lineX = env$analysis.lineX,
                   lineY= env$analysis.lineY)

    
  # cleaning
  # -----------
  
  if (env$clean.stage1Dur == "") env$clean.stage1Dur <- 80
  if (env$clean.stage1Dist == "") env$clean.stage1Dist <- 1
  if (env$clean.stage2Dur == "") env$clean.stage2Dur <- 40
  if (env$clean.stage2Dist == "") env$clean.stage2Dist <- 3
  if (env$clean.stage3 == "") env$clean.stage3 <- FALSE
  if (env$clean.stage3Dur == "") env$clean.stage3Dur <- 140
  if (env$clean.stage4 == "") env$clean.stage4 <- FALSE
  if (env$clean.stage4Min == "") env$clean.stage4Min <- 80
  if (env$clean.stage4Max == "") env$clean.stage4Max <- 800
  if (env$clean.delete == "") env$clean.delete <- FALSE
  if (env$clean.outlier == "") env$clean.outlier <- FALSE
  
  clean <- list(stage1Dur = env$clean.stage1Dur, 
                stage1Dist = env$clean.stage1Dist,
                stage2Dur = env$clean.stage2Dur, 
                stage2Dist = env$clean.stage2Dist,
                stage3 = env$clean.stage3,
                stage3Dur = env$clean.stage3Dur,
                stage4 = env$clean.stage4,
                stage4Min = env$clean.stage4Min,
                stage4Max = env$clean.stage4Max,
                delete = env$clean.delete,
                outlier = env$clean.outlier
                )

  
  # exclude
  # --------
  
  if (env$exclude.blink == "") env$exclude.blink <- FALSE
  if (env$exclude.nfix == "") env$exclude.nfix <- 3
  if (env$exclude.sac == "") env$exclude.sac <- 200
  exclude <- list(blink = env$exclude.blink,
                  nfix = env$exclude.nfix,
                  sac = env$exclude.sac)

    
  # merge setup slot
  # -----------------
  
  setup <- list(tracker = tracker, type = type, message = message, item = item,
                variable = variable, stimulus = stimulus, indicator = indicator, 
                display = display, font = font, clean = clean, 
                analysis = analysis, exclude = exclude)
  
  return(setup)
  
}
