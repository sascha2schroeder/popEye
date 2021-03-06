
# boundary plot
# --------------

PlotFastX <- function(exp, subject, trial, pdf = F, interactive = F, sub = F) {
  
  
  # start pdf
  if (sub == F) {
    if (pdf == T) {
      pdf("Test.pdf", width = 16, height = 8.5)
      par(mfrow = c(1, 1), cex = .9, oma = c(0, 0, 2, 0))
    } else {
      par(mfrow = c(1, 1), cex = 1.25, oma = c(0, 0, 3, 0))
      if (interactive == T) par(ask = T)
    }
  }
    
  tmp <- SelectSubjectTrial(exp, subject, trial)
  
  # data
  msg <- tmp$all
  fix <- tmp$fix
  fix$ys2 <- (fix$ys - exp$setup$display$marginY) / exp$setup$font$size + 
    exp$setup$display$marginY

  # variables
  x <- exp$setup$display$marginX
  y <- exp$setup$display$marginY 
  
  
  # basic plot
  # -----------
  
  fix1 <- fix[fix$start < msg$start[msg$msg == exp$setup$message$boundary],]
  fix2 <- fix[fix$start > msg$start[msg$msg == exp$setup$message$boundary],]
  
  # basic plot
  plot(fix$xs, fix$ys, 
       ylim = c(exp$setup$display$marginY - 1*exp$setup$font$size,
                exp$setup$display$marginY + 1*exp$setup$font$size), 
       xlim = c(0, exp$setup$display$resolutionX), type = "n", 
       pch = 16, xlab = "x Position (px)", ylab = "y Position (px)", 
       main = "X Plot")

  points(fix1$xs, fix1$ys2, type = "b", pch = 16, col = "cornflowerblue", 
         cex = fix$dur / mean(fix$dur))
  points(fix2$xs, fix2$ys2, type = "b", pch = 16, col = "navyblue", 
         cex = fix$dur / mean(fix$dur))
  
  # add blinks
  blink <- tmp$sac[tmp$sac$msg == "BLINK", ]
  blink$ys2 <- (blink$ys - exp$setup$display$marginY) / exp$setup$font$size + 
    exp$setup$display$marginY
  points(blink$xs, blink$ys2, type = "b", pch = 16, col = "red", 
         cex = blink$dur / mean(blink$dur))

  
  # boundary change
  # ----------------
  
  # compute change event
  boundary <- msg$start[msg$msg == exp$setup$message$boundary]
  for (i in 1:nrow(msg)) {
    if (is.na(msg$stop[i]) == T) next # FIX: change
    if (boundary %in% msg$start[i]:msg$stop[i]) {
      change <- msg[i, ]
    }
  }
  
  # saccade
  if (change$msg == "SAC"){
    change$ys2 <- (change$ys - y) / exp$setup$font$size + y
    change$ye2 <- (change$ye - y) / exp$setup$font$size + y
    arrows(change$xs, change$ys2, change$xe, change$ye2, col = "cyan3", code = 0,
           lwd = 2)
  }
  
  # fixation
  if (change$msg == "FIX"){
    change$ys2 <- (change$ys - y) / exp$setup$font$size + y
    points(change$xs, change$ys2, type = "b", pch = 16, col = "cyan3", cex = 2)
  }
  
  
  # prime
  # ------
  
  # compute prime event
  prime <- msg$start[msg$msg == exp$setup$message$prime]
  for (i in 1:nrow(msg)) {
    if (is.na(msg$stop[i]) == T) next # FIX: change
    if (prime %in% msg$start[i]:msg$stop[i]) {
      change.prime <- msg[i, ]
    }
  }
  
  # saccade
  if (change.prime$msg == "SAC"){
    change.prime$ys2 <- (change.prime$ys - y) / exp$setup$font$size + y
    change.prime$ye2 <- (change.prime$ye - y) / exp$setup$font$size + y
    arrows(change.prime$xs, change.prime$ys2, change.prime$xe, change.prime$ye2, col = "green", code = 0,
           lwd = 2)
  }
  
  # fixation
  if (change.prime$msg == "FIX"){
    change.prime$ys2 <- (change.prime$ys - y) / exp$setup$font$size + y
    points(change.prime$xs, change.prime$ys2, type = "b", pch = 16, col = "green", cex = 2)
  }
  
  
  # advanced plot
  # --------------
  
  # add letters
  letters <- unlist(strsplit(tmp$meta$text, ""))
  y <- exp$setup$display$marginY 
  
  for (j in 1:(length(tmp$meta$letter.boundary) - 1)) {
    rect(tmp$meta$letter.boundary[j], y - exp$setup$font$size / 2,
         tmp$meta$letter.boundary[j + 1], y + exp$setup$font$size / 2, angle = NA)
    if (is.element(letters[j], exp$setup$font$print$mi)){
      text((tmp$meta$letter.boundary[j] + tmp$meta$letter.boundary[j + 1] - 1) / 2,
           y + 0.25, labels = letters[j], family = exp$setup$font$family, cex = .9)
    } else if (is.element(letters[j], exp$setup$font$print$up)){
      text((tmp$meta$letter.boundary[j] + tmp$meta$letter.boundary[j + 1] - 1) / 2,
           y + 0.25, labels = letters[j], family = exp$setup$font$family, cex = .9)
    } else if (is.element(letters[j], exp$setup$font$print$de)){
      text((tmp$meta$letter.boundary[j] + tmp$meta$letter.boundary[j + 1] - 1) / 2,
           y + 0.15, labels = letters[j], family = exp$setup$font$family, cex = .9)
    } else if (is.element(letters[j], exp$setup$font$print$pu)){
      text((tmp$meta$letter.boundary[j] + tmp$meta$letter.boundary[j + 1] - 1) / 2,
           y, labels = letters[j], family = exp$setup$font$family, cex = .9)
    }
  }
  
  # add words
  for (j in 1:(length(tmp$meta$word.boundary) - 1)) {
    rect(tmp$meta$letter.boundary[tmp$meta$word.boundary[j] + 1], y - exp$setup$font$size / 2, 
         tmp$meta$letter.boundary[tmp$meta$word.boundary[j + 1]], y + exp$setup$font$size / 2, angle = NA, lwd = 2)
  }
  
  # add fixation number
  fix$num <- 1:nrow(fix)
  for (i in 1:nrow(fix)) {
    text(fix$xs[i], (fix$ys2[i] - 1), fix$num[i], col = "navyblue", cex = .75)  
  }

  # add target word
  j <- tmp$meta$target
  rect(tmp$meta$letter.boundary[tmp$meta$word.boundary[j]], y - exp$setup$font$size / 2,
       tmp$meta$letter.boundary[tmp$meta$word.boundary[j + 1]], y + exp$setup$font$size / 2, 
       angle = NA, lwd = 2, col = MakeTransparent("navyblue", alpha = .2))
  
  # add boundary
  abline(v = tmp$meta$boundary, col = "navyblue", lwd = 2)
 
  # turn off device  
  if (sub == F) {
    if (pdf == T) {
      title(paste("Trial", tmp$meta$trialnum, sep = " "), 
            outer = T, cex.main = 1.75)
      dev.off()
    } else {
      title(paste("Trial", tmp$meta$trialnum, sep = " "), 
            outer = T, cex.main = 2)
      par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = F)
    }
  }
}
