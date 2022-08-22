
# runH is the multiplier of line height for the vertical run threshold
# runW is the multiplier of line height for the horiziontal run threshold
# lineS is the multiplier of line height for runs that are considered the same line
# lineN is the multiplier of line height for maximum distance of runs that are considered adjacent line

Slice = function(fix, stim, env = parent.frame(n = 3)) {
  
  lineS <- env$exp$setup$assign$lineS
  lineN <- env$exp$setup$assign$lineN
    
  # plot(fix$xn, fix$yn, type="l", col="lightgrey", ylim=c(max(fix$yn), min(fix$yn)))
  line_Y <- tapply(stim$ym, stim$line, max) # contains the Y position of all lines
  lineHeight <- mean(diff(line_Y)) # contains the average vertical difference of lines
  
  # enumerate runs
  fix$distx <- fix$disty <- NA
  fix$distx[2:length(fix$distx)] <- diff(fix$xn)
  fix$disty[2:length(fix$disty)] <- diff(fix$yn)
  fix$run[1] <- 1
  
  runY = lineHeight * env$exp$setup$assign$lineY
  # runY = 0.25 * lineHeight * env$exp$setup$assign$lineY
  # runY = 0.25 * lineHeight * 2
  runX = 0.75 * env$exp$setup$font$width * env$exp$setup$assign$lineX
  # runX = 0.75 * 15 * 35
  for (i in 2:nrow(fix)) {
    fix$run[i] <- fix$run[i - 1] + (abs(fix$disty[i]) >= runY | abs(fix$distx[i]) >= runX)
  }
  fix$distx <- fix$disty <- NULL
  # for(r in unique(fix$run)) {
  #   lines(fix$xn[fix$run == r], fix$yn[fix$run == r], type="l", col="black")
  # }
  
  fix$l <- NA # add column for line number
  fix$orig <- T # keep information about original fixations
  nrMax <- which.max(lapply(lRuns(fix), FUN = function(r) return(max(r$xn) - min(r$xn))))
  fix$l[fix$run == nrMax] <- 0
  # lines(fix$xn[fix$l==0 & !is.na(fix$l)], fix$yn[fix$l==0 & !is.na(fix$l)], col="blue")
  fix$num = fix$start = fix$stop = fix$xs = fix$ys = fix$blink = fix$dur = NULL
  j = 0
  while(sum(is.na(fix$l)) > 0 & j < length(line_Y) * 2) {
    j = j + 1
    nrUp <- min(fix$l, na.rm = T)
    rds <- runDists(nrUp, fix)
    if (nrow(rds) > 0) {
      # same line
      fix$l[fix$run %in% rds$run[abs(rds$ym) < lineS * lineHeight]] <- nrUp
      # lines(fix$xn[fix$l==nrUp & !is.na(fix$l)], fix$yn[fix$l==nrUp & !is.na(fix$l)], col="blue")
      # one line up
      fix$l[fix$run %in% rds$run[rds$ym <= (lineS * -1) * lineHeight & rds$ym >= (lineN * -1) * lineHeight]] = nrUp - 1
      # lines(fix$xn[fix$l==(nrUp-1) & !is.na(fix$l)], fix$yn[fix$l==(nrUp-1) & !is.na(fix$l)], col="red")
      if (sum(fix$l == (nrUp - 1), na.rm = T) == 0 & sum(fix$yn < min(fix$yn[!is.na(fix$l)])) > 0) {
        fix = rbind(fix, 
                    data.frame(xn = c(mean(fix$xn)), 
                               yn = c(mean(fix$yn[fix$l == nrUp & !is.na(fix$l)]) - lineHeight),
                               orig = F, run = max(fix$run) + 1, l = nrUp - 1))
        # points(mean(fix$xn), mean(fix$yn[fix$l == nrUp & !is.na(fix$l)]) - lineHeight)
      }
    }
    nrDown <- max(fix$l, na.rm = T)
    rds <- runDists(nrDown, fix)
    if (nrow(rds) > 0) {
      # same line
      fix$l[fix$run %in% rds$run[abs(rds$ym) < lineS * lineHeight]] <- nrDown
      # lines(fix$xn[fix$l==nrDown & !is.na(fix$l)], fix$yn[fix$l==nrDown & !is.na(fix$l)], col="blue")
      # one line down
      fix$l[fix$run %in% rds$run[rds$ym >= lineS * lineHeight & rds$ym <= lineN * lineHeight]] <- nrDown + 1
      # lines(fix$xn[fix$l==(nrDown+1) & !is.na(fix$l)], fix$yn[fix$l==(nrDown+1) & !is.na(fix$l)], col="red")
      if (sum(fix$l == (nrDown + 1), na.rm = T) == 0 & sum(fix$yn > max(fix$yn[!is.na(fix$l)])) > 0) {
        fix <- rbind(fix, data.frame(xn = c(mean(fix$xn)), 
                                     yn = c(mean(fix$yn[fix$l == nrDown & !is.na(fix$l)]) + lineHeight), 
                                     orig = F, run = max(fix$run) + 1, l = nrDown + 1))
        # points(mean(fix$xn), mean(fix$yn[fix$l == nrDown & !is.na(fix$l)]) + lineHeight)
      }
    }
  }
  # missing line assignments
  missing <- is.na(fix$l)
  for (i in 1:sum(missing)) {
    fix$l[missing][i] <- which.min(abs(fix$yn[missing][i] - tapply(fix$yn, fix$l, mean))) + (min(fix$l, na.rm = T) - 1)
  }
  # prune thin lines
  while(max(fix$l) - min(fix$l) > length(line_Y) - 1) {
    if (sum(fix$l == max(fix$l) & fix$orig) > sum(fix$l == min(fix$l) & fix$orig)) {
      fix$l[fix$l == min(fix$l)] = min(fix$l) + 1
    } else {
      fix$l[fix$l == max(fix$l)] = max(fix$l) - 1
    } 
  }
  
  fix$l <- fix$l - min(fix$l) + 1
  # text(fix$xn, fix$yn, fix$l)
  fix$yn <- fix$l
  
  return(fix[fix$orig == T, ])
  
}

lRuns <- function(fix) {
  
  return(lapply(unique(fix$run), FUN = function(r) return(fix[fix$run == r, c("xn", "yn")])))
  
}

runDists <- function(nrLine, fix) {
  
  line <- fix[fix$l == nrLine & !is.na(fix$l), c("xn", "yn")]
  runs <- unique(fix$run[is.na(fix$l)])
  if (!length(runs)) return(data.frame(run = c(), ym = c()))
  ym <- c()
  for (r in runs) {
    ydists <- 0
    rps <- fix[fix$run == r, c("xn","yn")]
    for (i in 1:nrow(rps)) {
      p <- fix[fix$run == r, c("xn","yn")][i, ]
      ydists <- ydists + (p$yn - line$yn[which.min(abs(line$xn - p$xn))])
    }
    ym <- c(ym, ydists / nrow(rps))
  }
  
  return(data.frame(run = runs, ym = ym))
}
