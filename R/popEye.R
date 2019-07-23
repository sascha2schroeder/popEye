
popEye <- function(datpath, stimpath,
                   tracker.model, tracker.software, 
                   tracker.results,
                   type, 
                   message.start, message.stop,
                   message.boundary, message.prime, message.target,
                   variable.id, variable.cond,
                   item.practice, item.trigger,
                   item.question, item.keep,
                   item.pracnum,
                   stimulus.file, stimulus.id,
                   stimulus.cond, stimulus.preview,
                   stimulus.prime, stimulus.text, 
                   indicator.word, indicator.ia, indicator.target, indicator.line,
                   display.marginLeft, display.marginTop, display.marginRight, display.marginBottom, 
                   font.name, font.size, font.spacing,
                   analysis.eyelink, analysis.vfac, 
                   analysis.mindur, analysis.postdur,
                   analysis.drift, analysis.sparse,
                   analysis.driftX, analysis.driftY,
                   analysis.lineMethod,
                   clean.stage1Dur, clean.stage1Dist,
                   clean.stage2Dur, clean.stage2Dist,
                   clean.stage3, clean.stage3Dur, 
                   clean.stage4, clean.stage4Min,
                   clean.stage4Max, clean.delete, clean.outlier,
                   exclude.blink, exclude.nfix, exclude.sac,
                   outpath = "", outname = "",
                   debug.version = NULL,
                   debug.subject = NULL
) {
  
  
  # ----------------------------------
  # setup experiment 
  # ----------------------------------
  
  message("Initialize experiment")
  
  # create output file
  exp <- list(setup = NA, subjects = list())
  
  # exp <- list(setup = NA, subject = NA)
  # subject <- list()
  
  # retrieve setup infomation
  exp$setup <- SetupExperiment()
  
  # retrieve setup i  # any substructure (to be implemented)
  
  
  # create output files
  # --------------------
  
  # item files
  word.item <- data.frame(matrix(NA, 1, 8))
  colnames(word.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "wordnum", "word")
  
  ia.item <- data.frame(matrix(NA, 1, 8))
  colnames(ia.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "ianum", "ia")
  
  sent.item <- data.frame(matrix(NA, 1, 7))
  colnames(sent.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum", "sent")
  
  # fix
  fix <- NULL
  
  # sac
  sac <- NULL
  
  # results
  results <- list(text = NA, quest = NA)
  
  # clean
  if (type == "text") {
    clean <- data.frame(matrix(NA, 1, 10))
    colnames(clean) <- c("subid", 
                         "trialid", 
                         "trialnum", 
                         "itemid", 
                         "cond",
                         "trial.fix", 
                         "trial.blink", 
                         "trial.sac",
                         "trial.crit", 
                         "crit")
  } 
  
  if (type == "sentence") {
    clean <- data.frame(matrix(NA, 1, 10))
    colnames(clean) <- c("subid", 
                         "trialid", 
                         "trialnum", 
                         "itemid", 
                         "cond",
                         "trial.fix", 
                         "trial.blink", 
                         "trial.sac",
                         "trial.crit", 
                         "crit")
  } 
  
  if (type == "target") {
    clean <- data.frame(matrix(NA, 1, 20))
    colnames(clean) <- c("subid", 
                         "trialid", 
                         "trialnum", 
                         "itemid", 
                         "cond",
                         "trial.fix", 
                         "trial.blink", 
                         "trial.sac",
                         "trial.crit", 
                         "target.blink", 
                         "target.first",
                         "target.pre.sac", 
                         "target.pre.launch", 
                         "target.pre.refix",
                         "target.pre.reg", 
                         "target.post.fix",
                         "target.post.sac", 
                         "target.post.reg", 
                         "target.crit", 
                         "crit")
  }
  
  if (type == "boundary") {
    clean <- data.frame(matrix(NA, 1, 32))
    colnames(clean) <- c("subid", 
                         "trialid", 
                         "trialnum", 
                         "itemid", 
                         "cond",
                         "trial.fix", 
                         "trial.blink", 
                         "trial.sac",
                         "trial.crit", 
                         "target.blink",
                         "target.first", 
                         "target.pre.sac",
                         "target.pre.launch", 
                         "target.pre.refix",
                         "target.pre.reg", 
                         "target.post.fix",
                         "target.post.sac", 
                         "target.post.reg", 
                         "target.crit",
                         "boundary.trigger", 
                         "boundary.seq", 
                         "boundary.change.sac", 
                         "boundary.pre.time", 
                         "boundary.target.time", 
                         "boundary.post.time", 
                         "boundary.target.fix", 
                         "boundary.blink",
                         "boundary.pattern", 
                         "boundary.time",
                         "boundary.hook", 
                         "boundary.crit", 
                         "crit")
  }
  
  if (type == "fast") {
    clean <- data.frame(matrix(NA, 1, 33))
    colnames(clean) <- c("subid", 
                         "trialid", 
                         "trialnum", 
                         "itemid", 
                         "cond",
                         "trial.fix", 
                         "trial.blink", 
                         "trial.sac",
                         "trial.crit", 
                         "target.blink",
                         "target.first",
                         "target.pre.sac",
                         "target.pre.launch", 
                         "target.pre.refix",
                         "target.pre.reg", 
                         "target.post.fix",
                         "target.post.sac", 
                         "target.post.reg", 
                         "target.crit",
                         "fast.trigger", 
                         "fast.seq", 
                         "fast.sac.dur", 
                         "fast.pre.time", 
                         "fast.prime.time", 
                         "fast.post.prime", 
                         "fast.fix.dur", 
                         "fast.fix.target", 
                         "fast.blink",
                         "fast.pattern", 
                         "fast.time",
                         "fast.hook", 
                         "fast.crit", 
                         "crit")
  }
  # TODO: fast priming outdated
  
  
  # create version list
  # --------------------
  
  # check for version
  if (tracker.software == "EB") {
    if (length(grep("results", list.files(datpath))) == 0) {
      version.list <- list.files(datpath)
      version.list <- paste("/", version.list, "/", sep = "") # here ?
    } else {
      version.list <- ""
    }  
  } else if (tracker.software == "ET") {
    version.list <- ""
  }
  
  # initialize number of subjects
  nsub <- 0
  
  
  # ----------------------------------
  # version loop
  # ----------------------------------
  
  if (missing(debug.version) == T) {
    version.arg1 <- 1
    version.arg2 <- length(version.list)
  } else {
    version.arg1 <- debug.version
    version.arg2 <- debug.version
  }
  
  for (v in version.arg1:version.arg2) {
    
    # v <- 2
    
    # list of subjects
    if (tracker.software == "EB") {
      filepath <- paste(datpath, version.list[v], "results/", sep = "")  
      sub.list <- list.files(filepath)
    } else if (tracker.software == "ET") {
      filepath <- paste(datpath, version.list[v], sep = "")  
      sub.list <- list.files(filepath)
      sub.list <- sub.list[grep("asc", sub.list)]
    }
    
    
    # ----------------------------------
    # subject loop
    # ----------------------------------
    
    if (missing(debug.subject) == T) {
      subject.arg1 <- 1
      subject.arg2 <- length(sub.list)
    } else {
      subject.arg1 <- debug.subject
      subject.arg2 <- debug.subject
    }
    
    for (s in subject.arg1:subject.arg2) {
      
      # increment number of subjects
      nsub <- nsub + 1
      
      subid <- gsub("\\.asc", "", sub.list[s])
      
      # message subject
      message(paste(". Subject: ", subid, paste = ""))
      
      # generate header slot
      header <- list(subid = subid)
      
      # TODO: store other information about subject (e.g., version)
      
      
      # --------------------------------------------
      # Modul 1: Preprocessing
      # --------------------------------------------
      
      # message(". Modul 1: Preprocessing")
      
      
      # read data
      # -----------
      
      dat <-  ReadData(filepath, subid)
      
      # TODO: reading asc data rather slow
      # TODO: do not convert to asc, but read edf directly (-> external packages)
      
      
      # remove data
      # --------------
      
      message(".. Remove data")
      
      dat <- Remove(dat) 
      
      
      # create trials
      # ---------------
      
      message(".. Create trials")
      
      dat <- Preprocessing(dat)
      
      
      # -----------------------
      # Modul 2: Cleaning
      # -----------------------
      
      # message(". Modul 2: Cleaning")
      
      # add stimulus information
      # -------------------------
      
      message(".. Add stimulus")
      
      dat <- ReadStimulus(dat)
      
      
      # extract fixations
      # --------------------
      
      message(".. Extract fixations")
      
      dat <- ExtractFixation(dat)
      
      
      # assign letters/words
      # ---------------------
      
      message(".. Assign letters/words")
      
      dat <- MatchStim(dat)
      
      
      # clean IAs
      # -----------
      
      message(".. Cleaning interest area")
      
      dat <- CleanIA(dat)
      
      # NOTE: not sure whether this should be implemented here (or at all)
      # NOTE: stage3 cleaning is completely useless !
      # NOTE: stage4 cleaning is dangerous !
      # TODO: report deleted fixations
      
      
      # compute fixation measures
      # --------------------------
      
      message(".. Compute fixation measures")
      
      dat <- ComputeFixationMeasures(dat)
      
      
      # retrieve saccades and blinks
      # -----------------------------
      
      message(".. Compute saccade measures")
      
      dat <- ProcessSaccades(dat)
      
      
      # combine events
      # ----------------
      
      message(".. Combine events")
      
      dat <- CombineEvents(dat)
      
      
      # cleaning
      # ---------
      
      message(".. Cleaning trial")
      
      dat <- CleanAll(dat)
      
      # NOTE: think about relationship between cleaning here and in main analysis
      
      
      # clean trials
      # -------------
      
      if (exp$setup$analysis$sparse == TRUE) {
        dat <- Sparse(dat)
      }
      
      
      # finalize
      # ---------
      
      # names for trial slots
      for (i in 1:length(dat$trial)) {
        
        names(dat$trial)[i] <- paste("trial", i, sep = ".")
        # NOTE: select by trialid or itemid?
        
        fix <- rbind(fix, dat$trial[[i]]$fix)
        sac <- rbind(sac, dat$trial[[i]]$sac)
        
      }
      
      # save in experiment slot
      exp$subjects[[nsub]] <- list(header = header, trials = dat$trial)
      
      # names for subject slot
      names(exp$subjects)[nsub] <- paste("subject", subid, sep = ".")
      
      
      # -----------------------
      # Modul 3: Aggregation
      # -----------------------
      
      # message(". Modul 3: Aggregation")
      
      # item file
      # -----------
      
      message(".. Load item file")
      
      word.itemtmp <- ItemFileWord(dat)
      word.item <- rbind(word.item, word.itemtmp)
      
      ia.itemtmp <- ItemFileIA(dat)
      ia.item <- rbind(ia.item, ia.itemtmp)
      
      sent.itemtmp <- ItemFileSent(dat)
      sent.item <- rbind(sent.item, sent.itemtmp)
      
      
      # select fixations
      # -----------------
      
      # message(".. Collect fixations")
      # 
      # fixtmp <- SelectFix(dat)
      # # fixtmp$subid <-  subid
      # fix <- rbind(fix, fixtmp)
      # fix <- fix[order(fix$subid, fix$trialnum, fix$fixid), ]
      
      
      # # select saccades
      # # -----------------
      # 
      # message(".. Select saccades")
      # 
      # sactmp <- SelectSac(dat)
      # 
      # sactmp$subid <-  subid
      # sac <- rbind(sac, sactmp)
      # sac <- sac[order(sac$subid, sac$trialnum, sac$sacid), ]
      
      
      # results file
      # -------------
      
      # TODO: integration results file and experiment needs to be improved
      
      if (exp$setup$tracker$software == "EB" & exp$setup$tracker$results == T) {
        
        message(".. Load results file")
        
        resultstmp <- ResultsFile(subid,
                                  filepath)
        resultstmp$text$subid <-  subid
        resultstmp$quest$subid <-  subid
        results <- list(text = rbind(results$text, resultstmp$text),
                        quest = rbind(results$quest, resultstmp$quest))
      }
      
      
      # clean file
      # -------------
      
      message(".. Create clean file")
      
      cleantmp <- ComputeClean(dat)
      cleantmp$subid <- subid
      clean <- rbind(clean, cleantmp)
      
    }
    
  }
  
  # NOTE: save number of subjects in setup slot?
  
  
  # collect output files
  # ---------------------
  
  # item files
  exp$out$word.item <- word.item[-1, ]
  row.names(exp$out$word.item) <- NULL
  
  exp$out$ia.item <- ia.item[-1, ]
  row.names(exp$out$ia.item) <- NULL
  
  exp$out$sent.item <- sent.item[-1, ]
  row.names(exp$out$sent.item) <- NULL
  
  # fixations and saccades
  exp$out$fix <- fix
  row.names(exp$out$fix) <- NULL
  
  exp$out$sac <- sac
  row.names(exp$out$sac) <- NULL
  
  # results
  if (exp$setup$tracker$software == "EB" & exp$setup$tracker$results == T) {
    exp$out$results$text <- results$text[-1, ]
    row.names(exp$out$results$text) <- NULL
    exp$out$results$quest <- results$quest[-1, ]
    row.names(exp$out$results$quest) <- NULL
  }
  
  # out
  exp$out$clean <- clean[-1, ]
  row.names(exp$out$clean) <- NULL
  
  
  # aggregate word
  # ---------------
  
  message("Aggregate word")
  
  exp$out$wordfirst <- AggregateWordFirstrun(exp)
  exp$out$wordtmp <- AggregateWord(exp)
  exp <- CombineWord(exp)
  
  
  # aggregate IA
  # -------------
  
  message("Aggregate IA")
  
  exp$out$iafirst <- AggregateIAFirstrun(exp)
  exp$out$iatmp <- AggregateIA(exp)
  exp <- CombineIA(exp)
  
  # aggregate sentence
  # -------------------
  
  message("Aggregate sentence")
  
  exp$out$sentfirst <- AggregateSentenceFirstrun(exp)
  exp$out$senttmp <- AggregateSentence(exp)
  exp <- CombineSentence(exp)
  
  
  # aggregate trial
  # ----------------
  
  message("Aggregate trial")
  
  exp <- AggregateTrial(exp)
  
  
  # compute overview file
  # ----------------------
  
  message("Compute overview")
  
  exp <- ComputeOverview(exp)
  
  
  # clean up
  # ----------
  
  # exp$out$ia.item <- NULL
  # exp$out$word.item <- NULL
  # exp$out$sent.item <- NULL
  
  
  # save
  # -----
  
  message("Save")
  
  # set outpath
  if (outpath == "") {
    outpath <- getwd()
  }

  # set outname
  if (outname == "") {
    tmp <- unlist(strsplit(datpath, "/"))
    outname <- tmp[length(tmp)]
  }

  save
  saveRDS(exp, file = paste(outpath, "/", outname, ".RDS", sep = ""))
  
}
