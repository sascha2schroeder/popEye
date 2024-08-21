
buildpopEye <- function(inc = F, m = NULL) {
  
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
    
    splitTmp <- strsplit(curVer, "\\-")[[1]]
    splitVer <- strsplit(splitTmp, "\\.")[[1]]
    
    high <- splitVer[1]
    major <- splitVer[2]
    minor <- splitVer[3]
    patch <- splitTmp[2]
    
    incpatch <- as.character(as.numeric(patch) + 1)
    newVer <- paste(paste(high, major, minor, sep = "."), incpatch, sep = "-")
    
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
