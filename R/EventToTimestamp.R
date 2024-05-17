
EventToTimestamp <- function(dat, start) {

  # start saccade
  ssacc = matrix(NA, nrow(dat$sac), 6)
  ssacc[, 2] = "SSACC"
  for (i in 1:nrow(dat$sac)){
    ssacc[i, 1] = dat$sac$start[i] + start -1
  }
  ssacc = data.frame(ssacc, stringsAsFactors = F)
  colnames(ssacc) = c("time", "msg", "xs", "ys", "xe", "ye")
  
  # end saccade
  esacc = matrix(NA, nrow(dat$sac), 6)
  esacc[, 2] = "ESACC"
  for (i in 1:nrow(dat$sac)){
    esacc[i, 1] = dat$sac$stop[i] + start - 1
    esacc[i, 3] = dat$sac$xs[i]
    esacc[i, 4] = dat$sac$ys[i]
    esacc[i, 5] = dat$sac$xe[i]
    esacc[i, 6] = dat$sac$ye[i]
  }
  esacc = data.frame(esacc, stringsAsFactors = F)
  colnames(esacc) = c("time", "msg", "xs", "ys", "xe", "ye")
  
  # start fixation
  sfix = matrix(NA, nrow(dat$fix), 6)
  sfix[, 2] = "SFIX"
  for (i in 1:nrow(dat$fix)){
    sfix[i, 1] = dat$fix$start[i] + start -1
  }
  sfix = data.frame(sfix, stringsAsFactors = F)
  colnames(sfix) = c("time", "msg", "xs", "ys", "xe", "ye")
  
  # end fixation
  efix = matrix(NA, nrow(dat$fix), 6)
  efix[, 2] = "EFIX"
  for (i in 1:nrow(dat$fix)){
    efix[i, 1] = dat$fix$stop[i] + start - 1
    efix[i, 3] = dat$fix$xs[i]
    efix[i, 4] = dat$fix$ys[i]
  }
  efix = data.frame(efix, stringsAsFactors = F)
  colnames(efix) = c("time", "msg", "xs", "ys", "xe", "ye")

  if (nrow(dat$blink) > 0) {
    
    # start blinks
    sblink = matrix(NA, nrow(dat$blink), 6)
    sblink[, 2] = "SBLINK"
    for (i in 1:nrow(dat$blink)){
      sblink[i, 1] = dat$blink$start[i] + start -1
    }
    sblink = data.frame(sblink, stringsAsFactors = F)
    colnames(sblink) = c("time", "msg", "xs", "ys", "xe", "ye")
    
    # end blink
    eblink = matrix(NA, nrow(dat$blink), 6)
    eblink[, 2] = "EBLINK"
    for (i in 1:nrow(dat$blink)){
      eblink[i, 1] = dat$blink$stop[i] + start - 1
    }
    eblink = data.frame(eblink, stringsAsFactors = F)
    colnames(eblink) = c("time", "msg", "xs", "ys", "xe", "ye")
    
  }
  
  # combine and write out
  if (nrow(dat$blink) > 0) {
    out <- rbind(ssacc, esacc, sfix, efix, sblink, eblink)  
  } else {
    out <- rbind(ssacc, esacc, sfix, efix)  
  }
  
  out <- out[order(out$time), ]
  out$time <- as.numeric(out$time)
  out$xs <- as.numeric(out$xs)
  out$ys <- as.numeric(out$ys)
  out$xe <- as.numeric(out$xe)
  out$ye <- as.numeric(out$ye)

  return(out)
  
}
