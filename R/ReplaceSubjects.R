
ReplaceSubjects <- function(exp1, exp2) {
  
  sub <- exp2$out$subjects$subid

  exp <- DeleteSubjects(exp1, sub)
  exp <- AddSubjects(exp, exp2)
 
  return(exp)
  
}
