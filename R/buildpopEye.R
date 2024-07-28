
buildpopEye <- function(inc = F, date = F, m = NULL) {
  
  library(devtools)
  # setwd("~/popEye")
  oldwd <- getwd()
  setwd("~/popEye/popEye")
  
  if (inc == T) {
    incVer()
  }
  
  commitGit(m)
  build(path = "~/popEye")
  # install()
  document()
  setwd(oldwd)
  
}

incVer <- function(){
  
  f <- read.dcf(file="DESCRIPTION")
  
    curVer <- f[2]
    
    splitVer <- strsplit(curVer, "\\.")[[1]]
    
    high <- splitVer[1]
    major <- splitVer[2]
    minor <- splitVer[3]
    patch <- splitVer[4]
    
    incpatch <- as.character(as.numeric(patch) + 1)
    newVer <- paste(high, major, minor, incpatch, sep = ".")
    
    f[2] <- newVer
    f[3] <- format (Sys.time(), "%Y-%m-%d")
    
    write.dcf(f, "DESCRIPTION")
  
}

commitGit <- function(m = NULL){
  
  if (is.null(m) == F) {
    # print("commit")
    system("git status")
    system(paste("git add -A && git commit -m \"", m, "\"", sep = ""))
  } else {
    system("git status")
  }
  
  
}
