
ReadImages <- function(exp) {
  
  require(magick)
  
  dir <- exp$setup$analysis$datpath
 
  if (length(grep("^/", dir)) == 0) {
    tmpwd <- getwd()
    dir <- paste(tmpwd, dir, sep = "/")
  } 
  
  dir <- gsub("/$", "", dir)
  dirtmp <- list.files(dir, full.names = T)
  
  if (length(grep("runtime", dirtmp)) == 0) {
    
    dirtmp2 <- dirtmp[1]
    dirtmp2 <- list.files(dirtmp2, full.names = T)
    filepath <- dirtmp2[grep("runtime", dirtmp2)]
    
  } else {
    
    filepath <- dirtmp[grep("runtime", dirtmp)]
    
  }
  
  file <- paste(filepath, "/imagelst.dat", sep = "")
  
  tmp <- readLines(file)
  tmp2 <- tmp[grep("^[0-9]|^-", tmp)]
  tmp3 <- sapply(strsplit(tmp2, "_"), "[[", 2)
  tmp3 <- trimws(tmp3, which = "both")
  
  split <- strsplit(tmp3, " ")
  pos <- sapply(split, length)
  
  pic <- NULL
  text <- NULL
  for (i in 1:length(tmp3)) {
    pic[i] <- unlist(split[i])[pos[i]]
    text[i] <- paste(unlist(split[i])[1:(pos[i] - 1)], collapse = " ")
  }
  
  text <- gsub(exp$setup$indicator$target, "", text)
  if (exp$setup$indicator$word != "") {
    tet <- gsub(exp$setup$indicator$word, "", text)
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
  
  # retrieve item number
  for (i in 1:nrow(data)) {
    if (identical(agrep(data$text[i], stim_red), integer(0))) next
    data$number[i] <- agrep(data$text[i], stim_red)
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
