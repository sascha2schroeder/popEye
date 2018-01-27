
SetupExperiment <- function(env = parent.frame(n = 1)) {
  
  
  # setup tracker
  # --------------
  
  if (env$tracker.model == "") env$tracker.model <- "eyelink"
  if (env$tracker.software == "") env$tracker.software <- "EB"
  tracker <- list(model = env$tracker.model, software = env$tracker.software)
  
  
  # setup experiment type
  # ----------------------
  
  if (env$type == "") env$type <- "sentence"
  type <- env$type
  
  
  # messages
  # ---------
  
  message <- list(start = NA, stop = NA)  
  message$start <- env$message.start
  message$stop <- env$message.stop
  
  if (env$type == "boundary") {
    message$boundary <- env$message.boundary
    message$target <- env$message.target
  }

  if (env$message.itemid == "") env$message.itemid <- "id"
  message$itemid <- env$message.itemid
  if (env$message.condition == "") env$message.condition <- NA
  message$condition <- env$message.condition

  
  # stimfile
  # ---------
  
  
  if (env$stimulus.word == "") env$stimulus.word <- " "
  if (env$stimulus.target == "") env$stimulus.target <- "\\*"
  if (env$stimulus.ia == "") env$stimulus.ia <- NA
  
  stimulus <- list(file = env$stimulus.file,
                   word = env$stimulus.word,
                   target = env$stimulus.target,                 
                   ia = env$stimulus.ia)

    
  # setup display
  # --------------
  
  if (env$display.marginX == "") env$display.marginX <- 150
  if (env$display.marginY == "") env$display.marginY <- 300
  display <- list(marginX = env$display.marginX,
                  marginY = env$display.marginY)
  # TODO: add physical width/height/dist in cm (for visual angle calculations)
  # TODO: add aspect ration(4:3, 16:10, 16:9 for plots)
  
  
  # setup font
  # -----------
  
  if (env$font.name == "") env$font.name <- "CourierNew"
  if (env$font.size == "") env$font.size <- 14
  font <- list(name = env$font.name, size = env$font.size)
  
  # set font family
  if (env$font.name == "CourierNew") {
    font$family <- "mono"
  } else {
    font$family <- "unknown"
  }
  
  # pixel per letter
  if (font$name == "CourierNew" & font$size == 14) {
    font$letpix <- 13
  }
  
  # print classes
  font$print$up <- c("A","E","I","O","U","Q","W","R","T","Z","P","S","D","F",
                     "G","H","J","K","L","Y","X","C","V","B","N","M","Ä","Ö",
                     "Ü","d","f","h","k","l","t","b","ß","i","ä","ö","ü","?",
                     "!")
  font$print$mi <- c("v","w","r","z","s","x","c","n","m","a","e","o","u")
  font$print$de <- c("q","p","g","j","y")
  font$print$pu <- c(".",",")

  # TODO: multiline experiments: line spacing matrix (separat or in ymargin?)
  # TODO: experiment calculations 
  #       - calculate letpix based on font type and size information
  #       - load letsize table for non-mono fonts
  #       - delimiter characters (?)
  # TODO: create font.type x font.size table
  
  
  # analysis
  # -----------
  
  if (env$analysis.eyelink == "") env$analysis.eyelink <- FALSE
  if (env$analysis.vfac == "") env$analysis.vfac <- 5
  if (env$analysis.mindur == "") env$analysis.mindur <- 10
  if (env$analysis.postdur == "") env$analysis.postdur <- 30
  if (env$analysis.drift == "") env$analysis.drift <- TRUE
  if (env$analysis.sparse == "") env$analysis.sparse <- TRUE
  
  analysis <- list(eyelink = env$analysis.eyelink, vfac = env$analysis.vfac,
                   mindur = env$analysis.mindur, postdur = env$analysis.postdur,
                   drift = env$analysis.drift, sparse = env$analysis.sparse)
  
  
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
  
  clean <- list(stage1Dur = env$clean.stage1Dur, 
                stage1Dist = env$clean.stage1Dist,
                stage2Dur = env$clean.stage2Dur, 
                stage2Dist = env$clean.stage2Dist,
                stage3 = env$clean.stage3,
                stage3Dur = env$clean.stage3Dur,
                stage4 = env$clean.stage4,
                stage4Min = env$clean.stage4Min,
                stage4Max = env$clean.stage4Max,
                delete = env$clean.delete)

  
  # exclude
  # -----------
  
  if (env$exclude.blink == "") env$exclude.blink <- FALSE
  if (env$exclude.nfix == "") env$exclude.nfix <- 3
  exclude <- list(blink = env$exclude.blink,
                  nfix = env$exclude.nfix)

    
  # merge setup slot
  # -----------------
  
  setup <- list(tracker = tracker, type = type, message = message, 
                stimulus = stimulus, display = display, font = font, 
                clean = clean, analysis = analysis, exclude = exclude)
  
  return(setup)
  
}
