
SelectSubjectTrial <- function(exp, subject, trial) {
  
  out <- SelectTrialid(SelectSubject(exp, subject), trial)
  
  return(out)
  
}
