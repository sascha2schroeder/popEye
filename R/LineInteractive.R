
LineInteractive <- function(fix, stimmat) {
  
  # TODO: preselect answer
  # TODO: show previous assignments
  
  
  fix$line <- NA
  run <- as.numeric(unlist(dimnames(table(fix$run))))
  for (i in 1:length(run)) {
    # i <- 1
    
    # plot
    plot(stimmat$xs, stimmat$ym,
         xlim = c(0, max(stimmat$xe)), 
         ylim = c(max(stimmat$ye), min(stimmat$ys)),
         type = "n")
    
    lines <- as.numeric(unlist(dimnames(table(stimmat$line))))
    for (j in 1:max(lines)){
      rect(stimmat$xs[lines == j], stimmat$ye[lines == j], 
           stimmat$xe[lines == j], stimmat$ys[lines == j])  
      ym <- tapply(stimmat$ym, stimmat$line, max)
    }
    text(ym, labels = unlist(dimnames(ym)), col = "red")
    
    # assign line
    points(fix$xn, fix$yn, pch = 16, type = "l", cex = 1, col = "black")
    points(fix$xn[fix$run == run[i]], fix$yn[fix$run == run[i]],
           pch = 16, type = "b", cex = 1, col = "red")
    key <- readline(prompt = "Enter line number:")
    fix$line[fix$run == run[i]] <- as.numeric(key)
    
  }
  
  fix$type[fix$line == 0] <- "out"
  fix$line[fix$line == 0] <- NA
  
  return(fix)
  
}
