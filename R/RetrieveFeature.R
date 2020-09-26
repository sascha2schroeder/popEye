RetrieveFeature <- function(exp, subject, item, feature) {
  
  eval(parse(text = paste(deparse(substitute(exp)), 
                          "$subject$subject.", subject, 
                          "$item$item.", item, 
                          "$", feature, sep = "")))
}
