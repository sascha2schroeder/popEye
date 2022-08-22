
Merge <- function(fix, stimmat, env = parent.frame(n = 3)) {
  
  fix <- BuildSequences(fix)
  fix <- Phase1(fix, stimmat)
  fix <- Phase2(fix, stimmat)
  fix <- Phase3(fix, stimmat)
  fix <- Phase4(fix, stimmat)
  fix <- Phase5(fix, stimmat)
  fix <- AssignLine(fix, stimmat)
  
  return(fix)
  
}
