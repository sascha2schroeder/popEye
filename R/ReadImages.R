
ReadImages <- function(exp) {
  
  require(magick)
  
  dir <- exp$setup$analysis$datpath
  
  if (length(grep("^/", dir)) == 0 & length(grep("^~", dir)) == 0) {
    tmpwd <- getwd()
    dir <- paste(tmpwd, dir, sep = "/")
  } 
  
  dir <- gsub("/$", "", dir)
  dirtmp <- list.files(dir, full.names = T)
  
  if (length(grep("runtime", dirtmp)) == 0) {
    
    dirtmp2 <- dirtmp[1]
    dirtmp2 <- list.files(dirtmp2, full.names = T)
    filepath <- dirtmp2[grep("runtime$", dirtmp2)]
    
  } else {
    
    filepath <- dirtmp[grep("runtime$", dirtmp)]
    
  }
  
  file <- paste(filepath, "/imagelst.dat", sep = "")
  
  tmp <- readLines(file)
  tmp2 <- tmp[grep("^[0-9]|^-", tmp)]
  if (length(grep(".png", tmp2)) > 0) {
    tmp3 <- tmp2[-grep(".png", tmp2)]
  } else {
    tmp3 <- tmp2
  }
  
  tmp4 <- sapply(strsplit(tmp3, "_"), "[[", 2)
  tmp5 <- trimws(tmp4, which = "both")
  
  split <- strsplit(tmp5, " ")
  pos <- sapply(split, length)
  
  pic <- NULL
  text <- NULL
  for (i in 1:length(tmp5)) {
    pic[i] <- unlist(split[i])[pos[i]]
    text[i] <- paste(unlist(split[i])[1:(pos[i] - 1)], collapse = " ")
  }
  
  text <- gsub(exp$setup$indicator$target, "", text)
  if (exp$setup$indicator$word != "") {
    text <- gsub(exp$setup$indicator$word, "", text)
  }
  if (exp$setup$indicator$ia != " ") {
    text <- gsub(exp$setup$indicator$ia, "", text)
  }
  
  
  data <- data.frame(cbind(text, pic))
  colnames(data) <- c("text", "image")
  data$number <- NA
  
  # read stimfile and parse out indicators
  stim <- exp$setup$stimulus$file[, exp$setup$stimulus$text]
  stim_red <- stim
  stim_red <- gsub(exp$setup$indicator$target, "", stim_red)
  if (exp$setup$indicator$word != "") {
    stim_red <- gsub(exp$setup$indicator$word, "", stim_red)
  }
  if (exp$setup$indicator$ia != " ") {
    stim_red <- gsub(exp$setup$indicator$ia, "", stim_red)
  }
  if (exp$setup$separator$word != " ") {
    stim_red <- gsub(exp$setup$separator$word, "", stim_red)
  }
  if (exp$setup$separator$sentence != " ") {
    stim_red <- gsub(exp$setup$separator$sentence, "", stim_red)
  }
  
  # retrieve item number
  for (i in 1:nrow(data)) {
    # TODO: grep or agrep?
    tmpout <- agrep(data$text[i], stim_red)
    # tmpout <- grep(data$text[i], stim_red)
    if (length(tmpout) != 1) next
    data$number[i] <- tmpout
  }
  
  texts <- data[is.na(data$number) == F, ]
  texts <- texts[order(texts$number), ]
  
  # add images
  image_list <- list(number = NULL, file = NULL, image = list())
  
  image_list$number <- texts$number
  image_list$file <- texts$image
  
  # images <- NULL 
  for (i in 1:nrow(texts)) {
    file <- paste(filepath, "/images/", texts$image[i], ".png", sep = "")
    img <- magick::image_read(file)
    image_list$image[[i]] <- image_data(img, "rgba")
  }
  
  exp$setup$stimulus$images <- image_list
  
  return(exp)
  
}
