
PlotImage <- function(exp, item, plot = NULL, interactive = F, sub = F, cex = 1) {
  
  require(magick)
  
  # start plot
  if (sub == F) {
    if (missing(plot) == T) {
      par(mfrow = c(1, 1), cex = cex, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = T)
    } else {
      tmp <- unlist(strsplit(plot, "\\."))
      if (tmp[length(tmp)] == "pdf") {
        pdf(plot, width = 16, height = 8.5)
        par(mfrow = c(1, 1), cex = cex, oma = c(1, 0, 2, 0))
      } else if (tmp[length(tmp)] == "png") {
        png(plot, width = 2000, height = 1000)
        par(mfrow = c(1, 1), cex = cex, oma = c(1, 0, 2, 0))  
      }
    }
  }
  
  # plot(exp$setup$stimulus$images$image[[item]])
  plot(magick::image_read(exp$setup$stimulus$images$image[[item]]))
  
  # turn off device  
  if (sub == F) {
    
    if (missing(plot) == F) {
      dev.off()
    } else {
      par(mfrow = c(1, 1), cex = 1, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = F)
    }
  }
  
}

# PlotImage(exp, 3)
