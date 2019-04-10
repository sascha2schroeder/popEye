RetrieveFeature <- function(exp, subject, trial, feature) {
  
  # eval(parse(text = paste(deparse(substitute(var)), 
  #                         "$subject$subject.", deparse(substitute(subject)), 
  #                         "$trial$trial.", deparse(substitute(trial)), 
  #                         "$", deparse(substitute(feature)), sep = "")))
  
  eval(parse(text = paste(exp, 
                          "$subject$subject.", subject, 
                          "$trial$trial.", trial, 
                          "$", feature, sep = "")))
}