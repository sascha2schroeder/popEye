
CleanParse <- function(fix, sac, trial, env = parent.frame(n = 1)) {
  
names <- c("subid", "trialid", "trialnum", "itemid", "cond", "fixid", "start", "stop", "dur", "xn", "yn", "ps")
fix_sel <- fix[names]
fix_sel$msg <- "FIX"
fix_sel$xen <- NA
fix_sel$yen <- NA
fix_sel$id <- NA
fix_sel$sacid <- NA
names <- c("subid", "trialid", "trialnum", "itemid", "cond", "id", "fixid", "sacid", "msg", "start", "stop", "dur", "ps", "xn", "yn", "xen", "yen")
fix_sel <- fix_sel[names]
colnames(fix_sel) <- c("subid", "trialid", "trialnum", "itemid", "cond", "id", "fixid", "sacid", "msg", "start", "stop", "dur", "ps", "xs", "ys", "xe", "ye")

names <- c("subid", "trialid", "trialnum", "itemid", "cond", "sacid", "msg", "start", "stop", "dur", "xsn", "ysn", "xen", "yen")
sac_sel <- sac[names]
sac_sel$id <- NA
sac_sel$fixid <- NA
sac_sel$ps <- NA
names <- c("subid", "trialid", "trialnum", "itemid", "cond", "id", "fixid", "sacid", "msg", "start", "stop", "dur", "ps", "xsn", "ysn", "xen", "yen")
sac_sel <- sac_sel[names]
colnames(sac_sel) <- c("subid", "trialid", "trialnum", "itemid", "cond", "id", "fixid", "sacid", "msg", "start", "stop", "dur", "ps", "xs", "ys", "xe", "ye")

sel_comb <- rbind(fix_sel, sac_sel)
sel_comb <- sel_comb[order(sel_comb$start), ]
sel_comb$id <- 1:nrow(sel_comb)
row.names(sel_comb) <- 1:nrow(sel_comb)

return(sel_comb)

}
