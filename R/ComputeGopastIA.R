
ComputeGopastIA <- function(dat) {
  
  options(warn = -1)
  
  # create response vectors
  dat$gopast <- rep(NA, nrow(dat))
  dat$selgopast <- rep(NA, nrow(dat))
  
  # compute trialid within person 
  id <- dat$trialid
  ids <- unlist(dimnames(table(id)))
  
  # trial id
  for (i in 1:length(ids)){
  # i = 1
    
    # compute vector of IAs in trial
    ias <- as.numeric(unlist(dimnames(table(dat$ianum[id == ids[i]]))))
    
    # compute measures
    for (j in 1:length(ias)){
    # j = 1
      
        dat$gopast[id == ids[i]][dat$ianum[id == ids[i]]== ias[j]] <- 
          sum(
            dat$dur[id == ids[i]][
              dat$fixid[id == ids[i]] >= min(dat$fixid[id == ids[i]][dat$ianum[id == ids[i]] == ias[j]], na.rm = T) 
              & dat$fixid[id == ids[i]] < min(dat$fixid[id == ids[i]][dat$ianum[id == ids[i]] > ias[j]], na.rm = T)
              & is.na(dat$ianum[id == ids[i]]) == F
              ]
            , na.rm = T)
        
        dat$selgopast[id == ids[i]][dat$ianum[id == ids[i]]== ias[j]] <- 
          sum(
              dat$dur[id == ids[i]][
                dat$fixid[id == ids[i]] >= min(dat$fixid[id == ids[i]][dat$ianum[id == ids[i]] == ias[j]], na.rm = T)
                & dat$fixid[id == ids[i]] < min(dat$fixid[id == ids[i]][dat$ianum[id == ids[i]] > ias[j]], na.rm = T)
                & dat$ianum[id == ids[i]] == ias[j]
                & is.na(dat$ianum[id == ids[i]]) == F
                ]
              , na.rm = T)
        
    }
    
  }
  
  options(warn = 1)
  
  return(dat)
  
}
