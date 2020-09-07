
TransformQuad <- function(points, x.old, y.old, x.new, y.new, x.adj = T){
  
  row.names(x.old) <- row.names(y.old) <- NULL
  
  B <- cbind(x.old, y.old, rep(1, 4), rep(0, 4), rep(0, 4), rep(0, 4),
             -x.old * x.new, -y.old * x.new, rep(0, 4), rep(0, 4), rep(0, 4),
             x.old, y.old, rep(1, 4), -x.old * y.new, -y.old * y.new)
  B <- matrix(as.vector(t(B)), nrow = 8, ncol = 8, byrow = T)
  D <- cbind(x.new, y.new)
  D <- c(t(D))
  l <- solve(B) %*% D
  A <- matrix(c(l[1:8], 1), nrow = 3, ncol = 3, byrow = T)
  
  if (x.adj == F) {
    A[1,] <- c(1,0,0)
  }
  
  n <- apply(points, 1, function(x) { solve(A) %*% c(x, 1) } )
  n <- t(n)
  if (x.adj == T) {
    n[,1] <- n[,1] / n[,3]
  }
  n[,2] <- n[,2] / n[,3]
  n <- round(n[, 1:2])
  
  return(n)
  
}
