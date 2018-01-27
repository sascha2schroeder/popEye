
ComputeRun <- function(dat, trial) {
  # trial = 1
  
  # run
  dat$trial[[trial]]$fix$runid <- 1
  for (j in 2:nrow(dat$trial[[trial]]$fix)){
    # j <- 2
    if (dat$trial[[trial]]$fix$reg.in[j] == 1 & dat$trial[[trial]]$fix$reg.in[j - 1] != 1) {
      dat$trial[[trial]]$fix$runid[j] <- dat$trial[[trial]]$fix$runid[j - 1] + 1
    } else {
      dat$trial[[trial]]$fix$runid[j] <- dat$trial[[trial]]$fix$runid[j - 1]
    }
  }
  
  # fixid in IA
  dat$trial[[trial]]$fix$ia.fix = ave(dat$trial[[trial]]$fix$num, 
                                        dat$trial[[trial]]$fix$ia, FUN = rank)
  
  # runid in IA
  dat$trial[[trial]]$fix$id <- 
    paste(dat$trial[[trial]]$fix$ia, dat$trial[[trial]]$fix$runid, sep = ":")
  fix.tmp <- dat$trial[[trial]]$fix[duplicated(dat$trial[[trial]]$fix$id) == F, ]
  fix.tmp$ia.run <- ave(fix.tmp$runid, fix.tmp$ia, FUN = rank)
  dat$trial[[trial]]$fix <- merge(dat$trial[[trial]]$fix, 
                                  fix.tmp[c("id", "ia.run")], by = "id")
  dat$trial[[trial]]$fix$id <- NULL
  dat$trial[[trial]]$fix <- dat$trial[[trial]]$fix[order(dat$trial[[trial]]$fix$num), ]
  
  # fixnum in ia.run
  dat$trial[[trial]]$fix$id <- 
    paste(dat$trial[[trial]]$fix$ia, dat$trial[[trial]]$fix$ia.run, sep = ":")
  dat$trial[[trial]]$fix$ia.run.fix <- 
    ave(dat$trial[[trial]]$fix$num, dat$trial[[trial]]$fix$id, FUN = rank)
  # dat$trial[[trial]]$fix$ia.run.nfix <- 
  #   ave(dat$trial[[trial]]$fix$ia.run.fix, dat$trial[[trial]]$fix$id, FUN = max)
  # NOTE: compute later during firstrun aggregation
  dat$trial[[trial]]$fix$id <- NULL
  dat$trial[[trial]]$fix <- dat$trial[[trial]]$fix[order(dat$trial[[trial]]$fix$num), ]
  
  return(dat)
  
}
