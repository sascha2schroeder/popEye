
MoveFixations <- function(fix, stimmat) {
  
  # fit textbox
  textbox <- matrix(NA, max(stimmat$line), 4)
  textbox[,1] <- tapply(stimmat$xs, stimmat$line, min)
  textbox[,2] <- tapply(stimmat$xe, stimmat$line, max)
  textbox[,3] <- tapply(stimmat$ym, stimmat$line, min)
  
  texttl <- c(min(textbox[,1]), min(textbox[,3]))
  texttr <- c(max(textbox[,2]), min(textbox[,3]))
  textbl <- c(min(textbox[,1]), max(textbox[,3]))
  textbr <- c(max(textbox[,2]), max(textbox[,3]))
  
  text <- rbind(texttl, texttr, textbl, textbr)
  
  # fit quandrangle
  fixtmp <- fix[fix$type == "in", c("xn", "yn")]
  fixtmp$zx <- scale(fixtmp$xn)
  fixtmp$zy <- scale(fixtmp$yn)
  fixtmp$z <- fixtmp$zx * fixtmp$zy
  
  tl <- fixtmp[scale(fixtmp$xn) < 0 & scale(fixtmp$yn) < 0,]
  tl <- c(min(tl$xn), min(tl$yn))
  tr <- fixtmp[scale(fixtmp$xn) > 0 & scale(fixtmp$yn) < 0,]
  tr <- c(max(tr$xn), min(tr$yn))
  br <- fixtmp[scale(fixtmp$xn) > 0 & scale(fixtmp$yn) > 0,]
  br <- c(max(br$xn), max(br$yn))
  bl <- fixtmp[scale(fixtmp$xn) < 0 & scale(fixtmp$yn) > 0,]
  bl <- c(min(bl$xn), max(bl$yn))
  
  quad <- rbind(tl, tr, bl, br)
  
  # project quandrangle on textbox
  cfix <- TransformQuad(cbind(fix$xn, fix$yn), 
                        text[,1], text[,2], quad[,1], quad[,2],
                        x.adj = F)
  
  # return 
  fix$xn <- cfix[,1]
  fix$yn <- cfix[,2]
  
  return(fix)
  
}
