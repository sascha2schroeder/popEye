
# Check OCR

CheckLines <- function(exp, lang = "eng") {
  
  require(tesseract)
  
  if (lang %in% tesseract_info()$available == F) {
    
    # if (Sys.info()$sysname == "Linux") {
    #   print('You are on Linux. You should install the tesseract language package using your package manager (e.g., via "sudo yum install tesseract-ocr-fra")'
    # } else {
      tesseract_download(lang)
    # }
    
  }
  
  engine <- tesseract(lang)
  
  for (i in 1:length(exp$setup$stimulus$stimmat)) {
    
    stim <- exp$setup$stimulus$stimmat[[i]]
    ocr <- tesseract::ocr(exp$setup$stimulus$images$image[[i]], engine = engine)
    
    stim_split <- NULL
    for (j in 1:max(stim$line)) {
      stim_split[j] <- paste(stim$letter[stim$line == j], collapse = "")
    }
    ocr_split <- unlist(strsplit(ocr, "\n"))
    ocr_split <- ocr_split[nchar(ocr_split) > 0]
    
    line_stim <- as.matrix(tapply(stim$letline, stim$line, max))
    line_ocr <- sapply(ocr_split, nchar)
    
    line <- tapply(stim$line, stim$line, max)
    
    if (sum(line_stim != line_ocr) > 0) {
      print(paste("Trial ", i, ": PROBLEM", sep = ""))
      probs <- line[line_stim != line_ocr]
      print(paste(".Line:", paste(probs, collapse = ", ", sep = " ")))
      for (k in 1:length(probs)) {
        print(stim_split[probs[k]], sep = "")
        print(ocr_split[probs[k]], sep = "")
      }
      
    } else {
      print(paste("Trial ", i, ": OKAY", sep = ""))
    }
    
  }
  
}

# PlotStimulus2(exp, 1)
# PlotImage(exp, 1)
# PlotCompare(exp, 3)
