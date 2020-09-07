
TranslateXY <- function(tmp, fix) {
  
  trans <- diag(3)
  trans[1,3] <- tmp[1]
  trans[2,3] <- tmp[2]
  
  ident <- rep(1, nrow(fix))
  mat <- t(cbind(fix,ident))
  coords <- t(trans %*% mat)[,-3]
  
  return(coords)
  
}
