
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
                   outpath = "", outname = "") {
  
  
  # ----------------------------------
  # setup experiment 
  # ----------------------------------
  
  message("Initialize experiment")
  
  # create output file
  exp <- list(setup = NA, subject = list())
  
  # exp <- list(setup = NA, subject = NA)
  # subject <- list()
  
  # retrieve setup infomation
  exp$setup <- SetupExperiment()
  
  # TODO: think about workflow (directories, paths, subids, etc.)
  
  # Experiment Builder: assumes that complete experiment including
  # all subfolders are in datpath; in particular, each subject is assumed
  # to have its own results folder including the edf file, the stimulus file,
  # and the results file
  
  # EyeTrack: assumes one folder in which all EDF files are included without
  # any substructure (to be implemented)
  
  
  # create output files
  # --------------------
  
  word.item <- data.frame(matrix(NA, 1, 7))
  colnames(word.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "wordnum", "word")
  
  sent.item <- data.frame(matrix(NA, 1, 6))
  colnames(sent.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "sentnum")
  
  if (exp$setup$type == "text") {
    ia.item <- data.frame(matrix(NA, 1, 7))
    colnames(ia.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia")
  }
  
  if (exp$setup$type == "sentence") {
    ia.item <- data.frame(matrix(NA, 1, 7))
    colnames(ia.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia")
  } 
  
  if (exp$setup$type == "target") {
    ia.item <- data.frame(matrix(NA, 1, 8))
    colnames(ia.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia", "target")
  } 
  
  if (exp$setup$type == "boundary") {
    ia.item <- data.frame(matrix(NA, 1, 8))
    colnames(ia.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia", "target")
  }
  
  if (exp$setup$type == "fast") {
    ia.item <- data.frame(matrix(NA, 1, 8))
    colnames(ia.item) <- c("subid", "trialid", "trialnum", "itemid", "cond", "ianum", "ia", "target")
  }
  
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

  # TODO: save version in subject header
  
  
  # ----------------------------------
  # version loop
  # ----------------------------------
  
  for (v in 1:length(version.list)) {
  # for (v in 1:1) {
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
    
    for (s in 1:length(sub.list)){
    # for (s in 2:2) {
      
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
      exp$subject[[nsub]] <- list(header = header, trial = dat$trial)
      
      # names for subject slot
      names(exp$subject)[nsub] <- paste("subject", subid, sep = ".")
      
      
      # -----------------------
      # Modul 3: Aggregation
      # -----------------------

      # message(". Modul 3: Aggregation")


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

  exp$out$fix <- fix
  row.names(exp$out$fix) <- NULL

  exp$out$sac <- sac
  row.names(exp$out$sac) <- NULL

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


  # save
  # -----

  # NOTE: save one RDS file for complete experiment in directory from which
  #       function has been called; name is the same as the last directory in
  #       datpath (not sure whether this generally makes sense -> argument?)

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
