
CleanFixations <- function(out, dur.thresh, dist.thresh){
  
  # select events  
  fix <- ComputeDurDist(out[out$msg == "FIX", ])
  fix$num <- 1:nrow(fix)
  
  sac <- out[out$msg == "SAC" | out$msg == "BLINK", ]
  sac$num <- 1:nrow(sac)
  
  i <- 1
  while (i <= nrow(fix)) {
      
    merge.before = F
    merge.after = F
    
    if (fix$dur[i] <= dur.thresh) {
      
      # check fixation n - 1
      if (fix$num[i] > 1) {
        if (fix$dur[i - 1] > dur.thresh & fix$blink[i - 1] == 0 & fix$dist[i] <= dist.thresh) {
          merge.before = T
        }
      } 
       
      # check fixation n + 1
      if (fix$num[i] < nrow(fix)) {
        if (fix$dur[i + 1] > dur.thresh & fix$blink[i + 1] == 0 & fix$dist[i + 1] <= dist.thresh) {
          merge.after = T
        }
      } 
      
      # check merge.status
      if (merge.before == T & merge.after == F) merge = -1
      if (merge.before == F & merge.after == T) merge = 1
      if (merge.before == F & merge.after == F) merge = 0
      if (merge.before == T & merge.after == T) {
        if (fix$dur[i - 1] >= fix$dur[i + 1]) {
          merge = -1
        } else {
          merge = 1
        }
      }
      
    # close if above duration threshold
    } else {
      merge <- 0
    }
    
    # action
    if (merge == 0) i <- i + 1
    
    if (merge == -1) {
      
      fix$stop[i - 1] = fix$stop[i]
      fix$xs[i - 1] = round((fix$xs[i - 1] + fix$xs[i]) / 2)
      fix$ys[i - 1] = round((fix$ys[i - 1] + fix$ys[i]) / 2)
      
      fix = fix[-i, ]
      fix$num = 1:nrow(fix)
      
      start <- fix$start[i -1]
      stop <- fix$stop[i - 1]
      
      fix = ComputeDurDist(fix)
      sac = sac[(sac$start > start & sac$stop < stop) == F, ]
      sac$num <- 1:nrow(sac)
      
    } 
    
    if (merge == 1) {
      
      fix$start[i + 1] = fix$start[i]
      fix$xs[i + 1] = round((fix$xs[i] + fix$xs[i + 1]) / 2)
      fix$ys[i + 1] = round((fix$ys[i] + fix$ys[i + 1]) / 2)
      
      fix = fix[-i, ]
      fix$num = 1:nrow(fix)
      
      start <- fix$start[i]
      stop <- fix$stop[i]
      
      fix = ComputeDurDist(fix)
      sac = sac[(sac$start > start & sac$stop < stop) == F, ]
      sac$num <- 1:nrow(sac)
    }
  
  }
  
  # save
  names <- c("num", "start", "stop", "xs", "ys", "xe", "ye", "msg", "blink")
  out <- rbind(fix[names], sac[names])
  out <- out[order(out$start), ]
  out$num <- 1:nrow(out)
  
  # delete last fixation
  if (out$msg[nrow(out)] == "FIX" & 
      (out$stop[nrow(out)] - out$start[nrow(out)]) < dur.thresh) {
      out <- out[-nrow(out), ]
  }
  
  return(out)

}