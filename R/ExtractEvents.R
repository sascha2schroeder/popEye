
ExtractEvents <- function(dat){
  
  # extract events (saccades, fixations, blinks) from *.asc file
  dat <- dat[grep(paste(c("SSACC", "ESACC", "SFIX", "EFIX", "SBLINK", "EBLINK"),
                        collapse = "|"),  dat, useBytes=TRUE)]
  dat <- dat[nchar(dat) > 0]
      
  # SFIX
  time <- sapply(strsplit(dat[grep("SFIX", dat)], " "), "[[", 5)
  msg <- rep("SFIX", length.out = length(time))
  sf <- data.frame(cbind(time, msg), stringsAsFactors = F)
  sf$time <- as.numeric(sf$time)
  sf$xs <- NA
  sf$ys <- NA
  sf$xe <- NA
  sf$ye <- NA
  
  # EFIX
  time <- sapply(strsplit(dat[grep("EFIX", dat)], "\t"), "[[", 2)
  msg <- rep("EFIX", length.out = length(time))
  xs <- sapply(strsplit(dat[grep("EFIX", dat)], "\t"), "[[", 4)
  xs <- as.numeric(gsub(" ", "", xs))
  ys <- sapply(strsplit(dat[grep("EFIX", dat)],"\t"), "[[", 5)
  ys <- as.numeric(gsub(" ", "", ys))
  
  ef <- data.frame(cbind(time, msg, xs, ys), stringsAsFactors = F)
  ef$time <- as.numeric(ef$time)
  ef$msg <- as.character(ef$msg)
  ef$xs <- round(as.numeric(ef$xs))
  ef$ys <- round(as.numeric(ef$ys))
  ef$xe <- NA
  ef$ye <- NA
  
  # SSACC
  time <- sapply(strsplit(dat[grep("SSACC", dat)], " "), "[[", 4)
  msg <- rep("SSACC", length.out = length(time))
  ss <- data.frame(cbind(time, msg), stringsAsFactors = F)
  ss$time <- as.numeric(ss$time)
  ss$xs <- NA
  ss$ys <- NA
  ss$xe <- NA
  ss$ye <- NA
  
  # ESACC
  time <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 2)
  msg <- rep("ESACC", length.out = length(time))
  xs <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 4)
  xs <- as.numeric(gsub(" ", "", xs))
  ys <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 5)
  ys <- as.numeric(gsub(" ", "", ys))
  xe <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 6)
  xe <- as.numeric(gsub(" ", "", xe))
  ye <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 7)
  ye <- as.numeric(gsub(" ", "", ye))
  
  es <- data.frame(cbind(time, msg, xs, ys, xe, ye), stringsAsFactors = F)
  es$time <- as.numeric(es$time)
  es$msg <- as.character(es$msg)
  es$xs <- round(as.numeric(es$xs))
  es$ys <- round(as.numeric(es$ys))
  es$xe <- round(as.numeric(es$xe))
  es$ye <- round(as.numeric(es$ye))
  
  # SBLINK
  time <- sapply(strsplit(dat[grep("SBLINK", dat)], " "), "[[", 3)
  msg <- rep("SBLINK", length.out = length(time))
  sb <- data.frame(cbind(time,msg),stringsAsFactors=F)
  sb$time <- as.numeric(sb$time)
  sb$xs <- NA
  sb$ys <- NA
  sb$xe <- NA
  sb$ye <- NA
  
  # EBLINK
  time <- sapply(strsplit(dat[grep("EBLINK", dat)], "\t"), "[[", 2)
  msg <- rep("EBLINK", length.out = length(time))
  eb <- data.frame(cbind(time, msg), stringsAsFactors = F)
  eb$time <- as.numeric(eb$time)
  eb$msg <- as.character(eb$msg)
  eb$xs <- NA
  eb$ys <- NA
  eb$xe <- NA
  eb$ye <- NA
  
  # combine and write out
  out <- rbind(sf, ef, ss, es, sb, eb)
  out <- out[order(out$time), ]
  row.names(out) <- NULL
  
  return(out)

}
