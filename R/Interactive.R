
Interactive <- function(fix, stimmat, env = parent.frame(n = 3)) {
  
  fix <- BuildSequences(fix)
  fix <- SelectLine(fix, stimmat)
  fix <- LineInteractive(fix, stimmat)
  
  return(fix)
  
}
