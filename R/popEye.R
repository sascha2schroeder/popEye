
popEye <- function(datpath, stimpath,
                   tracker.model, tracker.software, type, 
                   message.start, message.stop,
                   message.boundary, message.target,
                   message.itemid, message.condition,
                   item.practice, item.trigger,
                   item.question, item.keep,
                   stimulus.file, stimulus.id,
                   stimulus.cond, stimulus.text,
                   stimulus.change, stimulus.word,
                   stimulus.target, stimulus.ia,
                   display.marginX, display.marginY, 
                   font.name, font.size,
                   analysis.eyelink, analysis.vfac, 
                   analysis.mindur, analysis.postdur,
                   analysis.drift, analysis.sparse,
                   clean.stage1Dur, clean.stage1Dist,
                   clean.stage2Dur, clean.stage2Dist,
                   clean.stage3, clean.stage3Dur, 
                   clean.stage4, clean.stage4Min,
                   clean.stage4Max, clean.delete,
                   exclude.blink, exclude.nfix,
                   outpath = "", 
                   outname = "") {
  
  
  # ----------------------------------
  # setup experiment 
  # ----------------------------------
  
  message("Initialize experiment")
  
  # create output file
  exp <- list(setup = NA, subject = list())
  
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
  
  # item
  item <- data.frame(matrix(NA, 1, 6))
  colnames(item) <- c("subid", "trialnum", "itemid", "cond", "ia", "word")
  
  # fix
  fix <- NULL
  
  # results
  results <- list(text = NA, quest = NA)
  
  # clean
  if (type == "sentence") {
    clean <- data.frame(matrix(NA, 1, 7))
    colnames(clean) <- c("subid", "trialnum", "cond",
                         "trial.fix", "trial.blink",
                         "trial.crit", "crit")
  } 
  
  if (type == "target") {
    clean <- data.frame(matrix(NA, 1, 19))
    colnames(clean) <- c("subid", "trialnum", "cond",
                         "trial.fix", "trial.blink",
                         "trial.crit", "target.fix", "target.blink",
                         "target.pre.sac", "target.pre.skip",
                         "target.pre.launch", "target.pre.refix",
                         "target.pre.reg", "target.post.fix",
                         "target.post.sac", "target.post.refix",
                         "target.post.reg", "target.crit", "crit")
  }
  
  if (type == "boundary") {
    clean <- data.frame(matrix(NA, 1, 31))
    colnames(clean) <- c("subid", "trialnum", "cond",
                         "trial.fix", "trial.blink",
                         "trial.crit", "target.fix", "target.blink",
                         "target.pre.sac", "target.pre.skip",
                         "target.pre.launch", "target.pre.refix",
                         "target.pre.reg", "target.post.fix",
                         "target.post.sac", "target.post.refix",
                         "target.post.reg", "target.crit",
                         "boundary.trigger", "boundary.seq", 
                         "boundary.change.sac", "boundary.pre.time", 
                         "boundary.target.time", "boundary.post.time", 
                         "boundary.target.fix", "boundary.blink",
                         "boundary.pattern", "boundary.time",
                         "boundary.hook", "boundary.crit", "crit")
  }
  
  
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
  
  for (v in 1:length(version.list)) {
    # v <- 1

    # list of subjects
    if (tracker.software == "EB") {
      filepath <- paste(datpath, version.list[v], "results/", sep = "")  
      sub.list <- list.files(filepath)
    } else if (tracker.software == "ET") {
      filepath <- paste(datpath, version.list[v], sep = "")  
      sub.list <- list.files(filepath)
      sub.list <- sub.list[grep("edf|EDF", sub.list)]
    }
     
    
    # ----------------------------------
    # subject loop
    # ----------------------------------
    
    for (s in 1:length(sub.list)) {
    # for (s in 2:2) {
      
      # increment number of subjects
      nsub <- nsub + 1
      
      subid <- gsub("\\.EDF", "", sub.list[s])
      
      # message subject
      message(paste(". Subject: ", subid, paste = ""))
      
      # generate header slot
      header <- list(subid = subid)

      # TODO: store other information about subject (e.g., calibration accuracy,
      #       version)
      # TODO: function to read out calibration accuracy
      
      
      # --------------------------------------------
      # Modul 1: Preprocessing
      # --------------------------------------------
      
      # message(". Modul 1: Preprocessing")
      
      # read data
      # -----------
      
      dat <-  ReadData(filepath, 
                       subid)
      
      # TODO: loading rather slow, might cause problems for following function
      # TODO: do not convert to asc, but read edf directly (-> external packages)
      
      
      # remove data
      # --------------
      
      message(".. Remove data")
      
      dat <- Remove(dat) 
      
      # TODO: remove non-adjecent trials (questions in EyeTrack)
      #       Use identifier in variable name? Load external file?
     
      
      # create trials
      # ---------------
      
      message(".. Create trials")

      dat <- Preprocessing(dat)
      
      # TODO: align y-axis

      
      # -----------------------
      # Modul 2: Cleaning
      # -----------------------
      
      # message(". Modul 2: Cleaning")
      
      # NOTE: ExperimentalBuilder organized in folders
      # NOTE: read in subject-specific item file or general item file and then match
      #       by itemid -> extract during preprocessing
      
      
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
     
      
      # assign IAs
      # ------------
      
      message(".. Assign interest areas")
      
      dat <- MatchIA(dat)
      
      # NOTE: IA is word as default
      # TODO: think about relationship between IA and word
      #       (can be sub- [morphological constituents] or super-lexical [phrases])
     
      
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
      
      # NOTE: fixation measures only on IA level (which is the word level at present)
      # NOTE: relationship between IA and word level entirely unclear:
      #       sepearat measures? aggregate from word level to IA level (works only
      #       with superlexical IAs)
      
      
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
      # TODO: write out DA file
      
      
      # clean trials
      # -------------
      
      if (exp$setup$analysis$sparse == TRUE) {
        dat <- Sparse(dat)
      }
      
      
      # save in experiment slot
      # ------------------------
      
      exp$subject[[nsub]] <- list(header = header, trial = dat$trial)
      
      
      # -----------------------
      # Modul 3: Aggregation
      # -----------------------
      
      # message(". Modul 3: Aggregation")
      
      
      # item file
      # -----------
      
      message(".. Load item file")
      
      itemtmp <- ItemFile(dat)
      itemtmp$subid <- subid
      item <- rbind(item, itemtmp)
      
      
      # TODO: created from stim slot -> load?
      
      
      # select fixations
      # -----------------
      
      message(".. Select fixations")
      
      fixtmp <- SelectFix(dat)
      fixtmp$subid <-  subid
      fix <- rbind(fix, fixtmp)
      
      # TODO: include word (as text) in fix file
      
      
      # results file
      # -------------
      
      if (exp$setup$tracker$software == "EB") {
        
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
  
  exp$out$item <- item[-1, ]
  row.names(exp$out$item) <- NULL
  
  exp$out$fix <- fix
  row.names(exp$out$fix) <- NULL
  
  # TODO: contingent on "EB"
  if (exp$setup$tracker$software == "EB") {
    exp$out$results$text <- results$text[-1, ]
    row.names(exp$out$results$text) <- NULL
    exp$out$results$quest <- results$quest[-1, ]
    row.names(exp$out$results$quest) <- NULL
  }
  
  # out
  exp$out$clean <- clean[-1, ]
  row.names(exp$out$clean) <- NULL
  
  
  # aggregate IA
  # -------------------
  
  message("Aggregate IA")
  
  exp$out$first <- AggregateFirstrun(exp)
  exp$out$iatmp <- AggregateIA(exp)
  exp <- CombineIA(exp)
  
  
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
  
  exp$out$item <- NULL
  
  
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
  
  # save
  saveRDS(exp, file = paste(outpath, "/", outname, ".RDS", sep = ""))
  
}
