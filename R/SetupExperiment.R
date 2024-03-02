
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
                   text = env$stimulus.text,
                   hyphenwrap = env$stimulus.hyphenwrap)
  
  # indicator
  indicator <- list(word = env$indicator.word,
                    ia = env$indicator.ia,
                    target = env$indicator.target,
                    line = env$indicator.line)                 
 
  # separator
  separator <- list(word = env$separator.word,
                    sentence = env$separator.sentence,
                    sentence2 = env$separator.sentence2)    
   
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
               spacing = env$font.spacing,
               wrap = env$font.wrap)
  
  if (env$font.name == "CourierNew" | env$font.name == "Consolas") {
    font$family <- "mono"
  } else if (env$font.name == "Symbol") {
    font$family <- "HersheySymbol"
  } else {
    font$family <- "unknown"
  }
  
  
  # Arial
  # ------
  
  # Arial, 14 pt
  if (font$name == "Arial" & font$size == 14) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(10,11,10,11,11,6,11,10,4,4,9,4,16,
               10,11,11,11,6,10,5,10,9,13,9,9,9,
               10,11,10,
               13,13,14,14,13,12,15,13,6,10,13,11,15,
               13,15,13,15,14,13,12,13,13,19,13,12,12,
               13,15,13,
               11,11,11,11,11,11,11,11,11,11,
               5,5,5,5,6,11,11,7,11,11,7,6,11,17,
               13,5,6,6,11,5,5,11,5,11,4,11,11,6,
               8,11,7,6,6,5,19,11,12)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 18
    font$width <- 10
    font$lead <- 4
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # Arial, 15 pt
  if (font$name == "Arial" & font$size == 15) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(11,11,10,11,11,6,11,10,4,4,10,4,16,
               10,11,11,11,7,10,6,10,9,15,9,10,9,
               11,11,10,
               13,13,14,14,13,12,16,13,6,10,13,11,17,
               13,16,13,16,14,13,12,13,13,19,13,14,12,
               13,16,13,
               11,11,11,11,11,11,11,11,11,11,
               6,6,6,6,7,11,11,8,12,12,7,6,11,18,
               13,6,7,7,11,6,6,12,6,11,4,12,12,6,
               8,11,8,7,7,6,20,11,12)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 19
    font$width <- 10.5
    font$lead <- 5
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # Arial, 16 pt
  if (font$name == "Arial" & font$size == 16) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(12,11,11,11,12,6,11,11,5,4,10,4,16,
               11,12,11,11,7,11,6,11,11,15,10,11,9,
               12,12,11,
               13,14,15,15,14,13,16,14,6,11,14,12,17,
               14,16,14,16,15,14,12,14,13,21,14,14,13,
               13,16,14,
               12,12,12,12,12,12,12,12,12,12,
               6,6,6,6,7,12,12,8,12,12,7,6,12,19,
               14,6,7,7,12,6,6,12,6,12,4,12,12,6,
               8,12,8,7,7,6,21,11,13)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 19
    font$width <- 11
    font$lead <- 5
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # Arial, 18 pt
  if (font$name == "Arial" & font$size == 18) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(13,14,12,14,13,7,14,14,5,6,12,6,20,
               14,13,14,14,8,12,7,14,11,17,11,12,12,
               13,13,14,
               15,16,17,17,16,15,19,17,6,12,16,13,19,
               17,19,16,19,17,16,14,17,15,23,15,16,15,
               15,19,17,
               13,13,13,13,13,13,13,13,13,13,
               7,7,7,7,8,13,13,9,14,14,9,8,13,21,
               16,7,8,8,13,7,7,14,7,13,5,14,14,6,
               10,13,12,8,8,7,24,14,15)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 22
    font$width <- 13
    font$lead <- 7
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # Arial, 20 pt
  if (font$name == "Arial" & font$size == 20) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(15,15,14,15,15,7,15,15,6,6,14,6,22,
               15,15,15,15,9,14,8,15,13,19,12,14,13,
               15,15,15,
               18,18,20,20,18,17,21,19,8,13,18,15,23,
               19,21,17,21,20,18,16,19,17,28,17,18,17,
               18,21,19,
               15,15,15,15,15,15,15,15,15,15,
               8,8,8,8,9,15,15,11,16,16,10,8,15,24,
               18,8,9,9,15,8,8,16,8,15,5,16,16,6,
               11,15,12,9,9,8,27,16,17)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 25
    font$width <- 14.5
    font$lead <- 6
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # Arial, 22 pt
  if (font$name == "Arial" & font$size == 22) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(16,16,15,16,16,8,16,16,7,7,14,7,25,
               16,16,16,16,10,15,8,16,13,21,13,13,14,
               16,16,16,
               19,19,21,21,19,18,23,21,7,15,19,16,23,
               21,23,19,23,21,19,19,21,19,30,19,19,18,
               19,23,21,
               16,16,16,16,16,16,16,16,16,16,
               8,8,8,8,10,16,16,11,17,17,10,9,16,26,
               19,8,10,10,16,8,8,17,8,16,6,17,17,8,
               12,16,14,10,10,8,29,17,18)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 27
    font$width <- 15.5
    font$lead <- 7
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # Arial, 24 pt
  if (font$name == "Arial" & font$size == 24) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(17,17,16,17,17,10,17,18,7,7,16,7,27,
               18,17,17,17,11,16,9,18,15,23,14,15,15,
               17,17,18,
               21,21,23,23,21,20,25,23,9,16,21,18,27,
               23,25,21,25,23,21,19,23,21,32,21,21,20,
               21,25,23,
               18,18,18,18,18,18,18,18,18,18,
               9,9,9,9,11,18,18,12,19,19,11,11,18,28,
               21,9,11,11,18,9,9,19,9,18,6,19,19,8,
               13,18,14,11,11,9,32,18,20)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 29
    font$width <- 17
    font$lead <- 8
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  
  # Calibri
  # -------
  
  # Calibri, 18 pt
  if (font$name == "Calibri" & font$size == 18) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\"", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß", "–")
    pixel <- c(12,13,10,13,12,7,11,13,6,6,11,6,19,
               13,13,13,13,8,9,8,13,11,17,10,11,9,
               12,13,13,
               14,13,13,15,12,11,15,15,6,8,12,10,21,
               16,16,12,16,13,11,12,15,14,21,12,12,11,
               14,16,15,
               12,12,12,12,12,12,12,12,12,12,
               6,6,6,6,7,12,12,12,12,12,10,8,11,17,
               16,9,7,7,12,7,7,12,9,12,5,12,12,11,
               8,12,12,8,8,13,5,21,13,13)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 26
    font$width <- 0
    font$lead <- 8
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # Calibri, 23 pt
  if (font$name == "Calibri" & font$size == 23) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\"", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß", "–", "\u202F")
    pixel <- c(15,16,13,16,15,9,15,16,7,7,14,7,25,
               16,16,16,16,11,12,10,16,14,22,13,14,12,
               15,16,16,
               18,17,17,19,15,14,20,19,8,10,16,13,27,
               20,21,16,21,17,14,15,20,18,28,16,15,15,
               18,21,21,
               16,16,16,16,16,16,16,16,16,16,
               8,8,8,8,9,15,15,15,15,15,12,10,14,23,
               21,11,9,9,16,10,10,15,12,15,7,15,15,14,
               11,16,15,10,10,16,7,28,16,27,7)
    pixel <- pixel - 1
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 26
    font$width <- 16
    font$lead <- 8
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  
  # CourierNew
  # -----------
  
  # CourierNew, 14 pt
  if (font$name == "CourierNew" & font$size == 14) {
    font$letpix <- data.frame(letter = " ", pixel = 11)
    font$height <- 16
    font$width <- 11
    font$lead <- 3
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  # CourierNew, 16 pt
  if (font$name == "CourierNew" & font$size == 16) {
    font$letpix <- data.frame(letter = " ", pixel = 13)
    font$height <- 17
    font$width <- 13
    font$lead <- 5
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  # CourierNew, 18 pt
  if (font$name == "CourierNew" & font$size == 18) {
    font$letpix <- data.frame(letter = " ", pixel = 14)
    font$height <- 20
    font$width <- 14
    font$lead <- 5
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  # CourierNew, 20 pt
  if (font$name == "CourierNew" & font$size == 20) {
    font$letpix <- data.frame(letter = " ", pixel = 16)
    font$height <- 22
    font$width <- 16
    font$lead <- 6
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  # CourierNew, 24 pt
  if (font$name == "CourierNew" & font$size == 24) {
    font$letpix <- data.frame(letter = " ", pixel = 20)
    font$height <- 26
    font$width <- 20
    font$lead <- 6
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  # CourierNew, 27 pt
  if (font$name == "CourierNew" & font$size == 27) {
    font$letpix <- data.frame(letter = " ", pixel = 22)
    font$height <- 29 
    font$width <- 22
    font$lead <- 8
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  
  # CourierHebrew 20 pt
  if (font$name == "CourierHebrew" & font$size == 20) {
    font$letpix <- data.frame(letter = " ", pixel = 16)
    font$height <- 22
    font$width <- 16
    font$lead <- 5
    font$right <- TRUE
    font$fixed <- TRUE
  }
  
  # CourierRussian 22 pt
  if (font$name == "CourierRussian" & font$size == 22) {
    font$letpix <- data.frame(letter = " ", pixel = 17)
    font$height <- 22
    font$width <- 17
    font$lead <- 8
    font$right <- FALSE 
    font$fixed <- TRUE
  }
  
  
  # Menlo
  # ------
  
  # Menlo, 14 pt
  if (font$name == "Menlo" & font$size == 14) {
    font$letpix <- data.frame(letter = " ", pixel = 22)
    font$height <- 35
    font$width <- 22
    font$lead <- 6
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  
  # Consolas
  # --------
  
  # Consolas, 10 pt
  if (font$name == "Consolas" & font$size == 10) {
    font$letpix <- data.frame(letter = " ", pixel = 11)
    font$height <- 18
    font$width <- 11
    font$lead <- 4
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  # Consolas, 18 pt
  if (font$name == "Consolas" & font$size == 18) {
    font$letpix <- data.frame(letter = " ", pixel = 13)
    font$height <- 22
    font$width <- 13
    font$lead <- 7
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  # Consolas, 20 pt
  if (font$name == "Consolas" & font$size == 20) {
    font$letpix <- data.frame(letter = " ", pixel = 15)
    font$height <- 24
    font$width <- 15
    font$lead <- 9
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  # Consolas, 22 pt
  if (font$name == "Consolas" & font$size == 22) {
    font$letpix <- data.frame(letter = " ", pixel = 16)
    font$height <- 25
    font$width <- 16
    font$lead <- 9
    font$right <- FALSE
    font$fixed <- TRUE
  } 
  
  # ConsolasRussian 20 pt
  if (font$name == "ConsolasRussian" & font$size == 20) {
    font$letpix <- data.frame(letter = " ", pixel = 15)
    font$height <- 24
    font$width <- 15
    font$lead <- 9
    font$right <- FALSE
    font$fixed <- TRUE
  }

  # ConsolasRussian 22 pt
  if (font$name == "ConsolasRussian" & font$size == 22) {
    font$letpix <- data.frame(letter = " ", pixel = 16)
    font$height <- 25
    font$width <- 16
    font$lead <- 9
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  # ConsolasGreek 20 pt
  if (font$name == "ConsolasGreek" & font$size == 20) {
    font$letpix <- data.frame(letter = " ", pixel = 15)
    font$height <- 24
    font$width <- 15
    font$lead <- 9
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  
  # LucidaConsole
  # --------------
  
  # Lucida Console, 15 pt
  if (font$name == "LucidaConsole" & font$size == 15) {
    font$letpix <- data.frame(letter = " ", pixel = 12)
    font$height <- 17
    font$width <- 12
    font$lead <- 1
    font$right <- FALSE
    font$fixed <- TRUE
  }
  
  
  # TimesNew Roman
  # ---------------
  
  # TimesNewRoman, 14 pt
  if (font$name == "TimesNewRoman" & font$size == 14) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(8,10,9,10,8,6,9,9,4,4,9,4,14,
               9,10,10,10,6,8,5,9,9,13,9,9,8,
               8,10,9,9,
               13,12,13,13,11,11,14,13,6,7,13,11,16,
               13,14,11,14,13,11,12,13,13,18,13,13,11,
               13,14,13,
               9,9,9,9,9,9,9,9,9,9,
               5,5,5,4,6,10,10,9,11,10,8,6,8,16,
               15,5,6,6,9,6,6,11,5,9,4,11,11,3,
               8,9,8,9,9,5,17,11)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 17
    font$width <- 10.5
    font$lead <- 6
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # TimesNewRoman, 15 pt
  if (font$name == "TimesNewRoman" & font$size == 15) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(9,10,9,10,9,6,9,9,6,6,9,6,14,
               9,10,10,10,7,8,5,9,10,15,9,9,8,
               9,10,9,
               14,13,13,14,12,11,14,14,6,8,14,11,17,
               14,14,11,14,13,11,12,13,13,18,14,14,11,
               14,14,13,
               10,10,10,10,10,10,10,10,10,10,
               5,5,5,6,7,10,10,9,11,11,8,6,9,17,
               15,6,7,7,10,7,7,11,5,9,4,11,11,3,
               8,10,8,10,10,5,18,11,10)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 18
    font$width <- 10
    font$lead <- 6
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # TimesNewRoman, 16 pt
  if (font$name == "TimesNewRoman" & font$size == 16) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä", "ö", "ü", "ß", "é", "è", "ï", "??",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä", "Ö", "Ü",
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
                " ", ".", ",", ";", ":", "-", "_", "#", "*", "+", "~", "\"", "!", "?",
                "%", "&", "/", "(", ")", "$", "[", "]", "=", "\\", "§", "'", "<",
                ">", "|", "°", "€", "^", "}", "{")  
    pixel <- c(9, 10, 9, 10, 9, 7, 10, 10,  6,  6, 10,  6, 16,
               10, 11, 10, 10,  7,  8,  6, 11, 10, 15, 10, 9, 9,  
               9, 11, 11, 10, 9, 9, 6, 6,
               14, 14, 14, 15, 13, 11, 15, 15,  6,  8, 15, 12, 18,
               15, 15, 12, 15, 14, 10, 12, 15, 15, 19, 14, 15, 12,
               15, 15, 15,
               11, 11, 11, 12, 11, 11, 12, 11, 11, 11,
               5, 5,  5,  5,  6, 7, 11, 11, 10, 12, 11, 8, 6, 10, 
               18, 16,  6, 7,  7, 11, 7, 7, 12, 6, 11, 4, 12,
               12, 3, 8, 11, 10, 10, 10)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 20
    font$width <- 10.5
    font$lead <- 5
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # TimesNewRoman, 18 pt
  if (font$name == "TimesNewRoman" & font$size == 18) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(11,12,11,12,11,8,11,12,6,6,12,6,18,
               12,12,12,12,8,9,7,12,12,17,12,12,10,
               11,12,12,
               17,16,16,17,15,13,17,17,8,9,17,15,21,
               17,17,14,17,16,12,14,16,16,23,17,17,14,
               17,17,16,
               12,12,12,12,12,12,12,12,12,12,
               6,6,7,6,8,12,12,12,14,13,10,8,10,20,
               19,7,8,8,12,8,8,14,7,12,4,14,14,5,
               9,12,10,12,12,12,6,22,14,12)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 22
    font$width <- 12
    font$lead <- 6
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # TimesNewRoman, 20 pt
  if (font$name == "TimesNewRoman" & font$size == 20) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(12,14,12,14,12,8,12,14,7,5,15,8,20,
               14,14,14,14,9,10,8,14,12,19,13,13,12,
               12,14,14,
               19,18,18,19,17,15,19,19,8,10,19,16,24,
               19,19,15,19,18,15,16,19,19,25,19,19,16,
               19,19,19,
               14,14,14,14,14,14,14,14,14,14,
               7,7,8,6,9,14,14,13,15,15,10,8,12,22,
               21,8,9,9,13,9,9,15,7,13,4,15,15,5,
               10,14,12,13,13,7,25,16,14)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 25
    font$width <- 14
    font$lead <- 7
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # TimesNewRoman, 22 pt
  if (font$name == "TimesNewRoman" & font$size == 22) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(12,14,12,14,13,9,13,14,8,8,14,8,22,
               14,14,14,14,9,11,8,14,14,20,14,13,13,
               12,14,14,
               20,18,19,21,18,17,20,20,10,11,20,17,26,
               21,20,16,20,19,16,18,21,21,27,21,21,17,
               20,20,21,
               14,14,14,14,14,14,14,14,14,14,
               7,7,8,8,9,15,15,15,16,16,10,8,12,24,
               23,8,10,10,15,9,9,16,8,14,4,16,16,5,
               11,14,14,14,14,7,27,17,14)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 26
    font$width <- 14.5
    font$lead <- 8
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # TimesNewRoman, 24 pt
  if (font$name == "TimesNewRoman" & font$size == 24) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\",", "\u00A7","'","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß")
    pixel <- c(14,16,14,16,14,10,15,15,9,9,16,9,23,
               15,16,16,16,11,11,9,15,15,23,15,15,14,
               14,16,15,
               22,21,21,23,20,18,23,23,11,13,22,19,28,
               23,23,18,23,21,18,19,22,23,29,23,23,19,
               22,23,22,
               16,16,16,16,16,16,16,16,16,16,
               8,8,9,9,10,16,16,15,18,17,13,11,14,27,
               25,9,11,11,16,11,11,18,8,16,5,18,18,5,
               12,16,15,15,15,8,29,18,17)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 29
    font$width <- 16
    font$lead <- 9
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # Symbol
  # ------
  
  # Symbol, 13 pt 
  # TODO: height/lead missing
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
    font$height <- NA
    font$width <- 15
    font$lead <- NA
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # Symbol, 14 pt
  # TODO: height/lead missing
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
    font$height <- NA
    font$width <- 15
    font$lead <- NA
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  
  # Tahoma
  # -------
  
  # Tahoma, 16 pt
  if (font$name == "Tahoma" & font$size == 16) {
    letter <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                "ä","ö","ü",
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                "Ä","Ö","Ü",
                "0","1", "2", "3", "4", "5", "6", "7", "8", "9",
                ".",",",";",":","-","\u20de", "_","#","*","+","~","\u0022","!","?","%",
                "&","/","(",")","$","[","]","=","\"","“", "”","\u00A7","'","’","<",">","|",
                "°","\u20AC","^","{","}","\u0020","@","\u00B5","ß", "\u0095", "\u00F7")
    pixel <- c(11,12,10,12,11,7,12,12,5,6,10,5,18,
               12,11,12,12,8,9,7,12,10,16,10,10,9,
               11,11,12,
               13,13,13,14,12,11,14,14,8,9,12,10,16,
               14,15,12,15,13,12,12,14,13,19,12,12,12,
               13,15,14,
               11,11,11,11,11,11,11,11,11,11,
               6,6,7,7,8,8,11,15,11,15,15,8,7,10,21,
               14,8,8,8,11,8,7,15,9,9,9,11,4,4,15,15,8,
               10,11,15,10,10,12,7,19,12,12,80)
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    font$height <- 21
    font$width <- 12
    font$lead <- 8
    font$right <- FALSE
    font$fixed <- FALSE
  }
  
  # TahomaKorean 24 pt
  if (font$name == "TahomaKorean" & font$size == 24) {
    font$letpix <- data.frame(letter = " ", pixel = 32)
    font$height <- 26
    font$width <- 32
    font$lead <- 10
    font$right <- FALSE
    font$fixed <- TRUE
    font$half <- c(" ","\u00a0","\u2014","\u002c","\u003a","\u002e","\u2018",
                   "\u2019","\u201c","\u201d","\u0028","\u0029","\u007e","\u0030",
                   "\u0031","\u0032","\u0033","\u0034","\u0035","\u0036","\u0037",
                   "\u0038","\u0039","\u0043","\u0049","\u004f","\u0054","\u0056")
  }
  
  # ChineseSimplified 8 pt
  if (font$name == "ChineseSimplified" & font$size == 8) {
    font$letpix <- data.frame(letter = " ", pixel = 20)
    font$height <- 21
    font$width <- 20
    font$lead <- 6
    font$right <- FALSE
    font$fixed <- TRUE
    font$half <- c("1", "9", "3", "6", "", "(", ")")
    font$wrap <- FALSE
  }
  
  # ChineseTraditional 10 pt
  if (font$name == "ChineseTraditional" & font$size == 10) {
    font$letpix <- data.frame(letter = " ", pixel = 27)
    font$height <- 25
    font$width <- 27
    font$lead <- 10
    font$right <- FALSE
    font$fixed <- TRUE
    font$half <- c("(", ")")
    font$wrap <- FALSE
  }
  
  if (env$tracker.software == "psychopy") {
    font$lead <- 0
  }
  
  
  # Hindi, 28 pt
  if (font$name == "HindiKokila" & font$size == 28) {
    
    letter <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", # digits
                "\u0905", "\u0906", "\u0907", "\u0908", "\u0909", "\u090a", "\u090f", "\u0910", "\u0911", "\u0913", "\u0914", # vowels
                "\u0915", "\u0916", "\u0917", "\u0918", "\u091a", "\u091b", "\u091c", "\u095b", "\u091d", "\u091e", "\u091f", 
                "\u0920", "\u095c", "\u0921", "\u0922", "\u095d", "\u0923", "\u0924", "\u0925", "\u0926", "\u0927", "\u0928", 
                "\u092a", "\u092b", "\u092c", "\u092d", "\u092e", "\u092f", "\u0930", "\u0932", "\u0935", "\u0936", "\u0937", "\u0938", "\u0939", # consonants
                "-", "\u20de", "\u2013", ",", ":", "\u0964", "'", "\u201c", "\u201d", " ", "\"", # punct
                "\u0901", "\u0902", "\u093c", "\u0941", "\u0942", "\u0943", "\u0947", "\u0948", "\u094d", # diacritic non-spacing 
                "\u093e", "\u093f", "\u0940", "\u0949", "\u094b", "\u094c" # diacritic spacing
    )
    
    pixel <- c(14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 
               22, 30, 12, 12, 14, 21, 14, 14, 30, 30, 30,
               20, 22, 15, 17, 19, 19, 18, 18, 18, 19, 14, 
               15, 16, 16, 15, 15, 18, 15, 17, 14, 17, 14, 
               14, 19, 16, 18, 16, 16, 10, 20, 16, 19, 15, 18, 14,
               10, 10, 14, 7, 8, 8, 5, 13, 13, 18, 13,
               0, 0, 0, 0, 0, 0, 0, 0, 0,
               7, 8, 8, 7, 7, 7)
    
    font$letpix <- data.frame(letter = letter, pixel = pixel)
    
    lig_letter <- c("\u0915", "\u0916", "\u0917", "\u0918", "\u091a", "\u091b", "\u091c", "\u095b", "\u091d", "\u091e", "\u091f", 
                    "\u0920", "\u095c", "\u0921", "\u0922", "\u095d", "\u0923", "\u0924", "\u0925", "\u0926", "\u0927", "\u0928", 
                    "\u092a", "\u092b", "\u092c", "\u092d", "\u092e", "\u092f", "\u0930", "\u0932", "\u0935", "\u0936", "\u0937", "\u0938", "\u0939"
    )
    
    lig_pixel <- c(14, 17, 7, 11, 14, 19, 14, 18, 13, 19, 14, 
                   15, 16, 16, 15, 15, 11, 10, 12, 14, 12, 8, 
                   8, 13, 12, 12, 10, 10, 10, 14, 10, 11, 9, 12, 13)
    font$lig_letpix <- data.frame(letter = lig_letter, pixel = lig_pixel)
    
    font$height <- 32
    font$width <- 0
    font$lead <- 0
    font$right <- FALSE
    font$fixed <- FALSE
    font$dia <- c("\u0901", "\u0902", "\u093c", "\u0941", "\u0942", "\u0943", 
                  "\u0947", "\u0948", "\u094d", "\u093e", "\u093f", "\u0940", 
                  "\u0949", "\u094b", "\u094c")
    
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
  analysis <- list(version = sessionInfo()$otherPkgs$popEye$Version,
                   datpath = env$datpath,
                   eyelink = env$analysis.eyelink, 
                   smooth = env$analysis.smooth,
                   vfac = env$analysis.vfac,
                   mindur = env$analysis.mindur, 
                   postdur = env$analysis.postdur,
                   drift = env$analysis.drift, 
                   sparse = env$analysis.sparse)
  
  # assign
  assign <- list(driftX = env$assign.driftX, 
                 driftY = env$assign.driftY,
                 outlier = env$assign.outlier,
                 outlierDist = env$assign.outlierDist,
                 moveMethod = env$assign.moveMethod,
                 moveX = env$assign.moveX,
                 moveY = env$assign.moveY,
                 lineMethod = env$assign.lineMethod,
                 lineX = env$assign.lineX,
                 lineY = env$assign.lineY,
                 lineS = env$assign.lineS,
                 lineN = env$assign.lineN)
  
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
  exclude <- list(nfix = env$exclude.nfix,
                  sac = env$exclude.sac)
  
  # write out
  setup <- list(tracker = tracker, type = type, message = message, item = item,
                variable = variable, stimulus = stimulus, indicator = indicator, 
                separator = separator, display = display, font = font, clean = clean, 
                analysis = analysis, assign = assign, exclude = exclude)
  
  return(setup)
  
}
