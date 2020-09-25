
SelectSubjectTrial <- function(dat, subname, trialid) {
  
  out <- SelectTrialid(SelectSubject(dat, subname), trialid)
  
  return(out)
  
}
