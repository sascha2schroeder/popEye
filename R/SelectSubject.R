
SelectSubject <- function(dat, subname) {
  sub <- dat$subjects[[which(unlist(lapply(lapply(dat$subjects, "[[", "header"), "[[", "subid") == subname))]]
  return(sub)
}
