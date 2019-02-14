
# stage 3 cleaning
# -----------------

# DataViewer description:
# STAGE 3. Data Viewer goes over the entire data and checks for interest areas
# which include at least three fixations smaller than the threshold duration and no
# fixations larger than the threshold duration. In such cases the shorter fixations are
# merged into the larger ones.
# default duration threshold = 140 ms

# NOTE: description in DataViewer is rather unclear
# TODO: only "check" step is implemented at present, not "merge" step
# TODO: include ia.run in criterion?
# TODO: works only on ia-level

CleanStage3 <- function(dat, trial, env = parent.frame(n = 2)) {
  
  # print(env$exp$setup$clean$stage3Dur)
  
  dat$trial[[trial]]$fix$merge.stage3 <- 0
  
  for (i in 1:length(table(dat$trial[[trial]]$fix$ianum))) {
    tmp <- dat$trial[[trial]]$fix[dat$trial[[trial]]$fix$ianum == 
                                    as.numeric(unlist(dimnames(table(dat$trial[[trial]]$fix$ianum))))[i], ]
    if (nrow(tmp) > 0) {
      if (length(tmp$dur < env$exp$setup$clean$stage3Dur) >= 3 &
          dat$trial[[trial]]$fix$merge.stage3[dat$trial[[trial]]$fix$ianum ==
                                              as.numeric(unlist(dimnames(table(dat$trial[[trial]]$fix$ianum))))[i]]) {
        # print(paste("Stage 3: Fixations on IA", i, "need to be merged", sep = " "))
      }  
    }
  }
  
  return (dat)
  
}