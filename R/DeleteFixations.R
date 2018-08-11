
# delete fixations
# -----------------

# DataViewer description
# If "Delete Fixations Outside Interest Area" box is checked, this will remove those
# fixations falling out side of any interest area. Important: If no interest area is
# defined, all fixations will be removed!

# NOTE: word used as interest area
# NOTE: revise word definition to include y position?

# to be used on trial-level (already selected)
DeleteFixations <- function(dat, trial) {
  
  dat$trial[[trial]]$fix <- 
    dat$trial[[trial]]$fix[is.na(dat$trial[[trial]]$fix$ianum) == F, ]
  
  return(dat)
  
}
