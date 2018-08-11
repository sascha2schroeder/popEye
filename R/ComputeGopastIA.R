
# ComputeGopastIA

# takes fixation out file as input
# returns gopast and selective gopast times for each fixation
# can then be aggregated

# NOTE: split in seperate functions?

# functional hierarchy
# -> AggregateIA
# -> -> CombineIA
# -> -> -> popEye

ComputeGopastIA <- function(dat) {
  
  # create response vectors
  dat$gopast <- rep(NA, nrow(dat))
  dat$selgopast <- rep(NA, nrow(dat))
  
  # compute trialid within person 
  id <- paste(dat$subid, dat$trialid, sep = ":")
  ids <- unlist(dimnames(table(id)))
  
  # trial id
  for (i in 1:length(ids)){
  # for (i in 1:1){
    # i = 1
    
    # compute vector of IAs in trial
    ias <- as.numeric(unlist(dimnames(table(dat$ianum[id == ids[i]]))))
    
    # compute measures
    for (j in 1:length(ias)){
    # for (j in 2:2){
      # j = 1
      
        dat$gopast[id == ids[i]][dat$ianum[id == ids[i]]== ias[j]] <- 
          sum(
            dat$dur[id == ids[i]][
              dat$fixid[id == ids[i]] >= min(dat$fixid[id == ids[i]][dat$ianum[id == ids[i]] == ias[j]]) 
              & dat$fixid[id == ids[i]] < min(dat$fixid[id == ids[i]][dat$ianum[id == ids[i]] > ias[j]])
              ]
            )
        
        dat$selgopast[id == ids[i]][dat$ianum[id == ids[i]]== ias[j]] <- 
          sum(
              dat$dur[id == ids[i]][
                dat$fixid[id == ids[i]] >= min(dat$fixid[id == ids[i]][dat$ianum[id == ids[i]] == ias[j]])
                & dat$fixid[id == ids[i]] < min(dat$fixid[id == ids[i]][dat$ianum[id == ids[i]] > ias[j]])
                & dat$ianum[id == ids[i]] == ias[j]
                ]
            )
        # print(j)
    }
    # print(i)
  }
  
  return(dat)
}
