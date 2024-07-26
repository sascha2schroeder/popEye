
# TODO: Make nice
# TODO: Additional function to loop through all items

PlotCompare <- function(exp, item, plot = NULL, interactive = F, cex = 1) {
  
  stimmat <- exp$setup$stimulus$stimmat[[item]]
  
  # start plot
  if (missing(plot) == T) {
    par(mfrow = c(1, 2), cex = cex, oma = c(0, 0, 0, 0), cex = 1)
    if (interactive == T) par(ask = T)
  } else {
    tmp <- unlist(strsplit(plot, "\\."))
    if (tmp[length(tmp)] == "pdf") {
      pdf(plot, width = 16, height = 8.5)
      par(mfrow = c(1, 2), cex = cex, oma = c(1, 0, 2, 0))
    } else if (tmp[length(tmp)] == "png") {
      png(plot, width = 2000, height = 1000)
      par(mfrow = c(1, 2), cex = cex, oma = c(1, 0, 2, 0))
    }
  }
  
  # basic plot
  plot(stimmat$xs, stimmat$ys, 
       xlim = c(0, exp$setup$display$resolutionX), 
       # ylim = c(max(stimmat$ye) + 1*exp$setup$font$size,
       #          min(stimmat$ys) - 1*exp$setup$font$size),
       ylim = c(exp$setup$display$resolutionY, 0),
       type = "n", bty = "n", axes = F, xlab = "", ylab = "",
       main = paste("Item", item)
  )
  
  # add letters
  letters <- stimmat$letter
  y <- exp$setup$display$marginTop 
  
  for (i in 1:nrow(stimmat)){
    text(stimmat$xm[i], stimmat$ym[i],
         stimmat$letter[i], family = "Courier", cex = .7)
    
    # rect(stimmat$xs[i], stimmat$ye[i], 
    #      stimmat$xe[i], stimmat$ys[i])  
    
    # rect(stimmat$xs[i], stimmat$ys[i] + 0.5*exp$setup$font$size, 
    #      stimmat$xe[i], stimmat$ye[i] - 0.5*exp$setup$font$size,
    #      border = "navyblue", col = MakeTransparent("cornflowerblue", alpha = .1))  
    
  }
  
  # add words
  words <- as.numeric(unlist(dimnames(table(stimmat$wordnum))))
  for (j in 1:max(words)) {
    rect(min(stimmat$xs[stimmat$wordnum == words[j]]),
         min(stimmat$ys[stimmat$wordnum == words[j]]),
         max(stimmat$xe[stimmat$wordnum == words[j]]),
         max(stimmat$ye[stimmat$wordnum == words[j]]),
         angle = NA, lwd = 2, border = "navyblue")
  }
  
  # plot(exp$setup$stimulus$images$image[[item]])
  plot(magick::image_read(exp$setup$stimulus$images$image[[item]]))
  
  
  # turn off device
  if (missing(plot) == F) {
    dev.off()
  } else {
    par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
    if (interactive == T) par(ask = F)
  }
  
}

# PlotCompare(exp, 8)