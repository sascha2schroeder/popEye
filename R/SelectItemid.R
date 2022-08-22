
SelectItemid <- function(sub, item) {
  
  tr <- sub$items[[which(lapply(lapply(sub$items, "[[", "meta"), "[[", "itemid") == item)]]
  
  return(tr)
  
}
