
# boundary plot
# --------------

PlotBoundaryX <- function(exp, subject, trial, pdf = F, interactive = F, sub = F) {
  
  
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
  fix$ys2 <- (fix$ys - exp$setup$display$marginY) / exp$setup$font$letpix + 
    exp$setup$display$marginY

  # variables
  x <- exp$setup$display$marginX
  y <- exp$setup$display$marginY 
  
  
  # basic plot
  # -----------
  
  fix1 <- fix[fix$start < msg$start[msg$msg == exp$setup$message$boundary],]
  fix2 <- fix[fix$start > msg$start[msg$msg == exp$setup$message$boundary],]
  
  # basic plot
  plot(fix$xs, fix$ys, ylim = c(exp$setup$display$marginY - 1.5*exp$setup$font$letpix, 
                                exp$setup$display$marginY + 1.5*exp$setup$font$letpix), 
       xlim = c(0, exp$setup$display$resolutionX), type = "n", 
       pch = 16, xlab = "x Position (px)", ylab = "y Position (px)", 
       main = "X Plot")

  points(fix1$xs, fix1$ys2, type = "b", pch = 16, col = "cornflowerblue", 
         cex = fix$dur / mean(fix$dur))
  points(fix2$xs, fix2$ys2, type = "b", pch = 16, col = "navyblue", 
         cex = fix$dur / mean(fix$dur))
  
  # add blinks
  blink <- tmp$sac[tmp$sac$msg == "BLINK", ]
  blink$ys2 <- (blink$ys - exp$setup$display$marginY) / exp$setup$font$letpix + 
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
  
  print(boundary)
  # saccade
  if (change$msg == "SAC"){
    change$ys2 <- (change$ys - y) / exp$setup$font$letpix + y
    change$ye2 <- (change$ye - y) / exp$setup$font$letpix + y
    arrows(change$xs, change$ys2, change$xe, change$ye2, col = "cyan3", code = 0,
           lwd = 2)
  }
  
  # fixation
  if (change$msg == "FIX"){
    change$ys2 <- (change$ys - y) / exp$setup$font$letpix + y
    points(change$xs, change$ys2, type = "b", pch = 16, col = "cyan3", cex = 2)
  }
  
  
  # add text
  # ---------
  
  words <- gsub(exp$setup$stimulus$word, " ", tmp$meta$text)
  letters <- unlist(strsplit(words, ""))
  x <- exp$setup$display$marginX
  y <- exp$setup$display$marginY 
  
  # add letters
  for (j in 1:length(letters)){
    if (letters[j] == " ") next
    rect(x + j * exp$setup$font$letpix - exp$setup$font$letpix, y - exp$setup$font$letpix / 2,
         x + j * exp$setup$font$letpix, y + exp$setup$font$letpix / 2, angle = NA)
    if (is.element(letters[j], exp$setup$font$print$mi)){
      text(x + j * exp$setup$font$letpix - exp$setup$font$letpix + exp$setup$font$letpix / 2, 
           y + 0.25, labels = letters[j], family = exp$setup$font$family, cex = .9)
    } else if (is.element(letters[j], exp$setup$font$print$up)){
      text(x + j * exp$setup$font$letpix - exp$setup$font$letpix + exp$setup$font$letpix / 2,
           y + 0.35, labels = letters[j], family = exp$setup$font$family, cex = .9)
    } else if (is.element(letters[j], exp$setup$font$print$de)){
      text(x + j * exp$setup$font$letpix - exp$setup$font$letpix + exp$setup$font$letpix / 2,
           y + 0.15, labels = letters[j], family = exp$setup$font$family, cex = .9)
    } else if (is.element(letters[j], exp$setup$font$print$pu)){
      text(x + j * exp$setup$font$letpix - exp$setup$font$letpix + exp$setup$font$letpix / 2,
           y, labels = letters[j], family = exp$setup$font$family, cex = .9)
    }
  }
  
  # add words
  let <- sapply(unlist(strsplit(words, " ")), nchar)
  rect(x, y - exp$setup$font$letpix / 2, x + let[1] * exp$setup$font$letpix, y + exp$setup$font$letpix / 2, angle = NA, lwd = 2)
  for (j in 2:length(let)){
    rect(x + (sum(let[1:(j - 1)]) + (j - 1)) * exp$setup$font$letpix, y - exp$setup$font$letpix / 2,
         x + (sum(let[1:j]) + (j - 1)) * exp$setup$font$letpix, y + exp$setup$font$letpix / 2, 
         angle = NA, lwd = 2)
  }
  
  # add fixation number
  fix$num <- 1:nrow(fix)
  for (i in 1:nrow(fix)) {
    text(fix$xs[i], (fix$ys2[i] - 1), fix$num[i], col = "navyblue", cex = .75)  
  }

  # add target word
  j <- tmp$meta$target
  rect(x + (sum(let[1:(j - 1)]) + (j - 1)) * exp$setup$font$letpix, y - exp$setup$font$letpix / 2,
       x + (sum(let[1:j]) + (j - 1)) * exp$setup$font$letpix, y + exp$setup$font$letpix / 2, 
       angle = NA, lwd = 2, col = makeTransparent("navyblue", alpha = .2))
  
  # add boundary
  abline(v = SelectSubject(exp, subject)$trial[[trial]]$stim$boundary, col = "navyblue", lwd = 2)
 
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
