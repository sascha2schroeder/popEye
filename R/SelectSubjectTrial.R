
SelectSubjectTrial <- function(dat, subname, trialnum) {
  
  out <- SelectTrialnum(SelectSubject(dat, subname), trialnum)
  
  return(out)
  
}
