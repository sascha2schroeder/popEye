
AssignLetters <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # NOTE: only works with monospaced font at present
  
  # variables  
  left.margin <- env$exp$setup$display$marginX 
  right.margin <- env$exp$setup$display$resolutionX
  letpix <- env$exp$setup$font$letpix
  let.boundary <- seq((left.margin - letpix), right.margin, by = letpix)
  
  dat$trial[[trial]]$fix$letter = NA
  for (j in 1:nrow(dat$trial[[trial]]$fix)) {
    for (k in 1:length(let.boundary)) {
      if(dat$trial[[trial]]$fix$xs[j] >= 
         let.boundary[k]) dat$trial[[trial]]$fix$letter[j] <- k - 1
    }
  }
  
  return(dat)

}
