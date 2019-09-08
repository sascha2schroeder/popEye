
SetupExperiment <- function(env = parent.frame(n = 1)) {
  
  
  # setup tracker
  tracker <- list(model = env$tracker.model, 
                  software = env$tracker.software,
                  results = env$tracker.results)
  
  # setup experiment type
  type <- env$type
  
  # messages
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
  variable <- list(id = env$variable.id,
               cond = env$variable.cond)
  
  # item
  item <- list(pracnum = env$item.pracnum,
               practice = env$item.practice,
               trigger = env$item.trigger,
               question = env$item.question)
  
  # stimfile
  stimulus <- list(file = env$stimulus.file,
                   id = env$stimulus.id,
                   cond = env$stimulus.cond,
                   preview = env$stimulus.preview,
                   prime = env$stimulus.prime,
                   text = env$stimulus.text)

  # indicator
  indicator <- list(word = env$indicator.word,
                    ia = env$indicator.ia,
                    target = env$indicator.target,
                    line = env$indicator.line)                 
  
  # display
  display <- list(marginLeft = env$display.marginLeft,
                  marginTop = env$display.marginTop,
                  marginRight = env$display.marginRight,
                  marginBottom = env$display.marginBottom)
  # TODO: add physical width/height/dist in cm (for visual angle calculations)
  # TODO: add aspect ration(4:3, 16:10, 16:9 for plots)
  
  
  # font
  # -----
  
  # TODO: specify height indicators for all fonts
  # NOTE: maybe in separate function
  
  # font type
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
    font$right <- F
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
    font$right <- F
  }
  
  # CourierNew, 18 pt
  if (font$name == "CourierNew" & font$size == 18) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!","–", "-","’","´", "%",":","‘","'","’",
                "ﬀ","ﬁ",
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
    pixel <- rep(14, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 20
    font$lead <- 5
    font$right <- F
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
    font$right <- F
  }
  
  # Consolas, 20 pt
  if (font$name == "Consolas" & font$size == 20) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!","–", "-","’","´","„","“",":","\"",";","”",
                "(", ")","'",
                "í", "ó", "É", "é", "á", "ñ", "ú",
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
    pixel <- rep(15, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 24
    font$lead <- 10
    font$right <- F
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
    font$right <- F
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
    font$right <- F
  }
  
  # Tahoma, 13 pt
  if (font$name == "Tahoma" & font$size == 13) {
    letter <- c("A","Ä","B","C","D","E","F","G","H","I","J","K","L","M","N","O","Ö",
                "P","Q","R","S","T","U","Ü","V","W","X","Y","Z",
                "a","ä","b","c","d","e","f","g","h","i","j","k","l","m","n","o","ö",
                "p","q","r","s","ß","t","u","ü","v","w","x","y","z",
                " ", ",",".","?","!","–", "-","’","´","„","“",":","\"",";","”",
                "(", ")","'",
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
    pixel <- rep(11, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 14
    font$lead <- 2
    font$right <- F
  }
  
  # ConsolasRussian 20 pt
  if (font$name == "ConsolasRussian" & font$size == 20) {
    letter <- c(" ", "-", "–", "—", ",", ":", "!", ".",  "«", "»", "(", ")",
                "1", "4", "7", "9", "а", "А", "б", "Б", "в", "В", "г", "Г", "д",
                "Д", "е", "Е", "ё", "ж", "Ж", "з", "З", "и", "И", "й", "к", "К",
                "л", "м", "М", "н", "Н", "о", "О", "п", "П", "р", "Р", "с", "С", 
                "т", "Т", "у", "У", "ф", "Ф", "х", "Х", "ц", "ч", "Ч", "ш", "Ш",
                "щ", "ъ", "ы", "ь", "э", "Э", "ю", "Ю", "я", "Я")
    pixel <- rep(15, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 24
    font$lead <- 9
    font$right <- F
  }
  
  # CourierHebrew 20 pt
  if (font$name == "CourierHebrew" & font$size == 20) {
    letter <- c(" ", "-", "–", "," , ":", ".", "'", "\"", "0", "1", "2", "8", "C", 
                "\u05d4", "\u05d3", "\u05d2", "\u05d1", "\u05d0", "\u05e5",
                "\u05e6", "\u05e3", "\u05e4", "\u05e2", "\u05e1", "\u05df",
                "\u05e0", "\u05dd", "\u05de", "\u05dc", "\u05da", "\u05db",
                "\u05d9", "\u05d8", "\u05d7", "\u05d6", "\u05d5", "\u05ea",
                "\u05e9", "\u05e8", "\u05e7")
    pixel <- rep(16, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 22
    font$lead <- 5
    font$right <- T
  }
  
  # TahomaArabic 13 pt
  if (font$name == "TahomaArabic" & font$size == 13) {
    letter <- c(
      # diacritics
      "\u064b",
      "\u064c",
      "\u064d",
      "\u064e",
      "\u064f",
      "\u0650",
      "\u0651",
      "\u0652",
      
      # punctuation
      " ",
      ",",
      "\u060c",
      "\u061b",
      ":",
      ".",
      "\"",
      "“",
      "«",
      "»",
      "(",
      ")",
      "*",
      "6",
      
      # regular characters
      "\u0626",
      "\u0625",
      "\u0624",
      "\u0623",
      "\u0622",
      "\u0621",
      "\u062D",
      "\u062C",
      "\u062B",
      "\u062A",
      "\u0629",
      "\u0628",
      "\u0627",
      "\u0634",
      "\u0633",
      "\u0632",
      "\u0631",
      "\u0630",
      "\u062F",
      "\u062E",
      "\u0641",
      "\u063A",
      "\u0639",
      "\u0638",
      "\u0637",
      "\u0636",
      "\u0635",
      "\u0648",
      "\u0647",
      "\u0646",
      "\u0645",
      "\u0644",
      "\u0643",
      "\u0642",
      "\u064A",
      "\u0649"
    )
    pixel <- rep(10, length(letter))
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 14
    font$lead <- 2
    font$right <- T
  }
  
  
  
  # print classes
  font$print$up <- c("A","E","I","O","U","Q","W","R","T","Z","P","S","D","F",
                     "G","H","J","K","L","Y","X","C","V","B","N","M","Ä","Ö",
                     "Ü","d","f","h","k","l","t","b","ß","i","ä","ö","ü","?",
                     "!")
  font$print$mi <- c("v","w","r","z","s","x","c","n","m","a","e","o","u")
  font$print$de <- c("q","p","g","j","y")
  font$print$pu <- c(".",",","–")

  
  # analysis
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
  exclude <- list(blink = env$exclude.blink,
                  nfix = env$exclude.nfix,
                  sac = env$exclude.sac)

    
  # write out
  setup <- list(tracker = tracker, type = type, message = message, item = item,
                variable = variable, stimulus = stimulus, indicator = indicator, 
                display = display, font = font, clean = clean, 
                analysis = analysis, exclude = exclude)
  
  return(setup)
  
}
