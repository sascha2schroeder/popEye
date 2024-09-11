
ExtractEvents <- function(infile, env = parent.frame(n = 2)) {
  
  if (env$exp$setup$tracker$model == "eyelink") {
    
  # extract events (saccades, fixations, blinks) from *.asc file
  dat <- infile[grep(paste(c("SSACC", "ESACC", "SFIX", "EFIX", "SBLINK", "EBLINK"),
                        collapse = "|"),  infile, useBytes=TRUE)]
  dat <- dat[nchar(dat) > 0]
  
  # SFIX
  time <- sapply(strsplit(dat[grep("SFIX", dat)], " "), "[[", 5)
  eye <- sapply(strsplit(dat[grep("SFIX", dat)], " "), "[[", 2)
  msg <- rep("SFIX", length.out = length(time))
  sf <- data.frame(cbind(time, eye, msg), stringsAsFactors = F)
  sf$time <- as.numeric(sf$time)
  sf$xs <- NA
  sf$ys <- NA
  sf$xe <- NA
  sf$ye <- NA
  sf$ps <- NA
  sf$amp <- NA
  sf$pv <- NA
  
  # EFIX
  time <- sapply(strsplit(dat[grep("EFIX", dat)], "\t"), "[[", 2)
  eye <- sapply(strsplit(sapply(strsplit(dat[grep("EFIX", dat)], "\t"), "[[", 1), " "), "[[", 2)
  msg <- rep("EFIX", length.out = length(time))
  xs <- sapply(strsplit(dat[grep("EFIX", dat)], "\t"), "[[", 4)
  xs <- as.numeric(gsub(" ", "", xs))
  ys <- sapply(strsplit(dat[grep("EFIX", dat)],"\t"), "[[", 5)
  ys <- as.numeric(gsub(" ", "", ys))
  ps <- sapply(strsplit(dat[grep("EFIX", dat)],"\t"), "[[", 6)
  ps <- as.numeric(gsub(" ", "", ps))
  
  ef <- data.frame(cbind(time, eye, msg, xs, ys, ps), stringsAsFactors = F)
  ef$time <- as.numeric(ef$time)
  ef$msg <- as.character(ef$msg)
  ef$xs <- round(as.numeric(ef$xs), 1)
  ef$ys <- round(as.numeric(ef$ys), 1)
  ef$xe <- NA
  ef$ye <- NA
  ef$ps <- round(as.numeric(ef$ps))
  ef$amp <- NA
  ef$pv <- NA
  
  # SSACC
  time <- sapply(strsplit(dat[grep("SSACC", dat)], " "), "[[", 4)
  eye <- sapply(strsplit(dat[grep("SSACC", dat)], " "), "[[", 2)
  msg <- rep("SSACC", length.out = length(time))
  ss <- data.frame(cbind(time, eye, msg), stringsAsFactors = F)
  ss$time <- as.numeric(ss$time)
  ss$xs <- NA
  ss$ys <- NA
  ss$xe <- NA
  ss$ye <- NA
  ss$ps <- NA
  ss$amp <- NA
  ss$pv <- NA
  
  # ESACC
  time <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 2)
  eye <- sapply(strsplit(sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 1), " "), "[[", 2)
  msg <- rep("ESACC", length.out = length(time))
  xs <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 4)
  xs <- as.numeric(gsub(" ", "", xs))
  ys <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 5)
  ys <- as.numeric(gsub(" ", "", ys))
  xe <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 6)
  xe <- as.numeric(gsub(" ", "", xe))
  ye <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 7)
  ye <- as.numeric(gsub(" ", "", ye))
  amp <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 8)
  amp <- as.numeric(gsub(" ", "", amp))
  pv <- sapply(strsplit(dat[grep("ESACC", dat)], "\t"), "[[", 9)
  pv <- as.numeric(gsub(" ", "", pv))
  
  #es <- data.frame(cbind(time, eye, msg, xs, ys, xe, ye), stringsAsFactors = F)
  es <- data.frame(cbind(time, eye, msg, xs, ys, xe, ye, amp, pv), stringsAsFactors = F)
  es$time <- as.numeric(es$time)
  es$msg <- as.character(es$msg)
  es$xs <- round(as.numeric(es$xs), 1)
  es$ys <- round(as.numeric(es$ys), 1)
  es$xe <- round(as.numeric(es$xe), 1)
  es$ye <- round(as.numeric(es$ye), 1)
  es$ps <- NA
  es$amp <- round(as.numeric(es$amp), 2)
  es$pv <- round(as.numeric(es$pv))
  
  # SBLINK
  if (sum(grepl("SBLINK", dat)) > 0) {
    
    time <- sapply(strsplit(dat[grep("SBLINK", dat)], " "), "[[", 3)
    eye <- sapply(strsplit(dat[grep("SBLINK", dat)], " "), "[[", 2)
    msg <- rep("SBLINK", length.out = length(time))
    sb <- data.frame(cbind(time, eye, msg),stringsAsFactors=F)
    sb$time <- as.numeric(sb$time)
    sb$xs <- NA
    sb$ys <- NA
    sb$xe <- NA
    sb$ye <- NA
    sb$ps <- NA
    sb$amp <- NA
    sb$pv <- NA
    
  } else {
    
    sb <- data.frame(matrix(ncol = 10, nrow = 0))
    colnames(sb) <- c("time", "eye", "msg", "xs", "ys", "xe", "ye", "ps", "amp", "pv")
    
  }
  
  # EBLINK
  if (sum(grepl("EBLINK", dat)) > 0) {
    
    time <- sapply(strsplit(dat[grep("EBLINK", dat)], "\t"), "[[", 2)
    eye <- sapply(strsplit(sapply(strsplit(dat[grep("EBLINK", dat)], "\t"), "[[", 1), " "), "[[", 2)
    msg <- rep("EBLINK", length.out = length(time))
    eb <- data.frame(cbind(time, eye, msg), stringsAsFactors = F)
    eb$time <- as.numeric(eb$time)
    eb$msg <- as.character(eb$msg)
    eb$xs <- NA
    eb$ys <- NA
    eb$xe <- NA
    eb$ye <- NA
    eb$ps <- NA
    eb$amp <- NA
    eb$pv <- NA
  
  } else {
    
    eb <- data.frame(matrix(ncol = 10, nrow = 0))
    colnames(eb) <- c("time", "eye", "msg", "xs", "ys", "xe", "ye", "ps", "amp", "pv")
    
  }
  
  # combine and write out
  out <- rbind(sf, ef, ss, es, sb, eb)
  out <- out[order(out$time), ]
  row.names(out) <- NULL
  
  return(out)
  
  } else if (env$exp$setup$tracker$model == "gazepoint") {
    
    # fixations
    fix <- infile[["data_collection/events/eyetracker/FixationStartEvent"]]
    fix <- fix[]
    fix <- fix[duplicated(fix$time) == F, ]
    
    # SFIX
    time <- round(fix$time*1000)
    eye <- NA
    msg <- rep("SFIX", length.out = length(time))
    sf <- data.frame(cbind(time, eye, msg), stringsAsFactors = F)
    sf$time <- as.numeric(sf$time)
    sf$xs <- NA
    sf$ys <- NA
    sf$xe <- NA
    sf$ye <- NA
    
    # EFIX
    time <- round(fix$time*1000)
    eye <- NA
    msg <- rep("EFIX", length.out = length(time))
    xs <- fix$gaze_x
    xs <- as.numeric(gsub(" ", "", xs))
    ys <- fix$gaze_y
    ys <- as.numeric(gsub(" ", "", ys))
    
    ef <- data.frame(cbind(time, eye, msg, xs, ys), stringsAsFactors = F)
    ef$time <- as.numeric(ef$time)
    ef$msg <- as.character(ef$msg)
    ef$xs = round((env$exp$setup$display$resolutionX / 2) + as.numeric(ef$xs))
    ef$ys = round((env$exp$setup$display$resolutionY / 2) - as.numeric(ef$ys))
    ef$xe <- NA
    ef$ye <- NA
    
    # # SSACC
    ss <- NA
    
    # # ESACC
    es <- NA
    
    # SBLINK
    sb <- NA
    
    # EBLINK
    eb <- NA
    
    # combine and write out
    out <- rbind(sf, ef, ss, es, sb, eb)
    out <- out[is.na(out$time) == F, ]
    out <- out[order(out$time), ]
    row.names(out) <- NULL
    
    return(out)
    
  }

}
