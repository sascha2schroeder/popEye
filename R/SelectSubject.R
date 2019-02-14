
SelectSubject <- function(dat, subname) {
  sub <- dat$subject[[which(unlist(lapply(lapply(dat$subject, "[[", "header"), "[[", "subid") == subname))]]
  return(sub)
}
