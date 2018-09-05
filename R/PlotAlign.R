
PlotAlign <- function(exp, subject, trial, pdf = F, interactive = F, sub = F) {
  
  # TODO: resize y dimension?
  # TODO: align letters 
  # TOtDO: make nice
  
  # start pdf
  if (sub == F) {
    if (pdf == T) {
      pdf("Test.pdf", width = 16, height = 8.5)
      par(mfrow = c(1, 1), cex = .9, oma = c(0, 0, 2, 0))
    } else {
      par(mfrow = c(1, 1), cex = 1.25, oma = c(0, 0, 0, 0))
      if (interactive == T) par(ask = T)
    }
  }
  
  tmp <- SelectSubjectTrial(exp, subject, trial)
  fix <- tmp$fix
  stimmat <- tmp$meta$stimmat
  
  # basic plot
  plot(fix$xs, fix$ys, 
       xlim = c(0, exp$setup$display$resolutionX), 
       ylim = c(max(tmp$meta$stimmat$ye) + 1*exp$setup$font$size,
                min(tmp$meta$stimmat$ys) - 1*exp$setup$font$size), 
       type = "n",
       main = "Aligned Fixations", xlab = "x Position (px)", ylab = "y Position (py)")
  for (i in 1:nrow(stimmat)){
    rect(stimmat$xs[i], stimmat$ye[i], stimmat$xe[i], stimmat$ys[i])  
    text(stimmat$xm[i], stimmat$ym[i], stimmat$letter[i], family = "Courier", cex = .9)
  }
  
  # original and corrected fixations
  points(fix$xs[fix$type == "in"], fix$ys[fix$type == "in"], cex = .75, type = "p", col = "blue", pch = 16)
  points(fix$xn[fix$type == "in"], 
         fix$yn[fix$type == "in"], 
         col = fix$line[fix$type == "in"], 
         pch = 16, type = "b")
  arrows(fix$xs[fix$type == "in"], fix$ys[fix$type == "in"], 
         fix$xn[fix$type == "in"], fix$yn[fix$type == "in"],
         code = 2, length = .07 )
  
  # add fixation number
  for (i in 1:nrow(fix)) {
    text(fix$xn[i], fix$yn[i] - 7, labels = fix$num[i],
         col = "royalblue", cex = .75)
  }
}
