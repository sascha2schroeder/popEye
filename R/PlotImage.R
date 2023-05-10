
PlotImage <- function(exp, item, plot = NULL, interactive = F, sub = F, cex = 1) {
  
  require(magick)
  
  plot(exp$setup$stimulus$images$image[[item]])
  
}

# PlotImage(exp, 3)
