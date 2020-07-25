
PlotBoundaryTime <- function(exp, subject, trial, pdf = F, interactive = F, 
                             sub = F, cex = 1) {
  
  # start pdf
  if (sub == F) {
    if (missing(pdf) == T) {
      par(mfrow = c(1, 1), cex = cex, oma = c(0, 0, 3, 0))
      if (interactive == T) par(ask = T)
    } else {
      pdf(pdf, width = 16, height = 8.5)
      par(mfrow = c(1, 1), cex = cex, oma = c(0, 0, 2, 0))
    }
  }
  
  tmp <- SelectSubjectTrial(exp, subject, trial)
  
  # compute boundary time and location
  boundary.time <- tmp$all$start[tmp$all$msg == exp$setup$message$boundary]
  target.time <- tmp$all$start[tmp$all$msg == exp$setup$message$target]
  boundary.loc <- as.numeric(tmp$meta$boundary)
  
  # compute offsets
  offset.time <- 50
  offset.loc <- 130
  
  # TODO: change this
  
  if (boundary.time - offset.time > 0) {
    # create plot
    if (sub == T) {
      plot(tmp$xy$time[(boundary.time - offset.time):(boundary.time + offset.time)],
           tmp$xy$x[(boundary.time - offset.time):(boundary.time + offset.time)], 
           type = "l", ylim = c(boundary.loc + offset.loc, boundary.loc - offset.loc), 
           main = "Time Plot", xlab = "Time (ms)", ylab = "x Position (px)")
    } else {
      plot(tmp$xy$time[(boundary.time - offset.time):(boundary.time + offset.time)],
           tmp$xy$x[(boundary.time - offset.time):(boundary.time + offset.time)], 
           type = "l", ylim = c(boundary.loc + offset.loc, boundary.loc - offset.loc),  
           main = paste("Trial", trial, sep = " "), 
           xlab = "Time (ms)", ylab = "x Position (px)", xaxt = "none")
      axis(1, at = seq((boundary.time - offset.time), (boundary.time + offset.time), by = 10), 
           tick = T)
    }
    
    # add lines
    for (i in 1:nrow(tmp$meta$stimmat)) {
      abline(h = tmp$meta$stimmat$xs[i], cex = .5)
    }
    
    # compute fixations
    fix <- tmp$fix
    fix.before <- tail(fix$stop[fix$start < boundary.time], n = 1)
    fix.after <- head(fix$start[fix$start > boundary.time], n = 1)
    
    for (i in 1:nrow(fix)){
      lines(fix$start[i]:fix$stop[i],
            rep(fix$xs[i], fix$stop[i] - fix$start[i] + 1),
            col = "cornflowerblue", lwd = 2)
    }
    
    abline(v = fix.before, lty = 1, col = "cornflowerblue", lwd = 2)
    abline(v = fix.after, lty = 1, col = "cornflowerblue", lwd = 2)
    
    # change saccade
    rect(fix.before, 0, fix.after, exp$setup$display$resolutionX,
         angle = NA, lwd = 2, col = MakeTransparent("cornflowerblue", alpha = .2))
    
    # add text
    letters <- unlist(strsplit(tmp$meta$text, ""))
    x <- exp$setup$display$marginLeft
    y <- exp$setup$display$marginTop
    
    # add boundary
    abline(h = boundary.loc, col = "navyblue", lwd = 2)
    
    # change saccade
    rect(boundary.time, 0, target.time, exp$setup$display$resolutionX,
         angle = NA, lwd = 2, col = MakeTransparent("navyblue", alpha = .2))
  }
  
  
  # turn off device  
  # ----------------
  
  # turn off device
  if (sub == F) {
    if (missing(pdf) == T) {
      title(paste("Trial", tmp$meta$trialid,
                  sep = " "), outer = T, cex.main = 2)
      par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = F)
      
    } else {
      title(paste("Trial", tmp$meta$trialid,
                  sep = " "), outer = T, cex.main = 1.75)
      dev.off()
    }
  }
  
}
