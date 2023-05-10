# Output files and eye movement measures

At the end of the preprocessing process, an RDS file is created. You can read in using `readRDS()`, e.g.

`myexp <- readRDS("somePath/someFolder/expFile.RDS")`

In this file, the raw data (x and y position for each time stamp, messages, stimulus information, parsed gaze events, etc.) are available. However, most of this information will typically be used only internally by popEye (e.g., for creating plots etc.) although users can retrieve it to do more advanced analysis. Most users will presumably want to work with data frames in which eye movement events have already been aggregated. This is similar to the reports provided SR's Data Viewer or the different output files from EyeDoctor. popEye provides such reports on different levels (participant, trial, interest area, etc.) and in each report different eye movement measures are provided that are specific to the level of analysis.  

In the following, I will describe the different reports and the measures that are provided within them. After that I will briefly explain how you can extract the raw data from the RDS file in case you are interested in them.

## Reports

All reports are stored in a specific slot of the RDS file which is called `reports`. Which reports are generated depends on the type of experiment that you have been running and that you specified when running `popEye()` (e.g., `type = "sentence"`). For example, text and sentence level variables are only provided if the type of the experiment is `text`. You can see which types are available for your experiment by using 

`str(myexp$reports, max.level = 1)` 

In the following, I will go through all reports from the highest to the lowest level.

You can extract and store the individual reports in a new R data frame (e.g., the subject report using `sub <- myexp$reports$subject`).

 
### Subject report

This data frame reports summary data on the level of the individual participant. It is available for every type of experiment and probably most useful to check your data and to decide whether some participants should be excluded (e.g., because eye tracking quality was very low or they show strange reading behavior). In addition, it will help you when you are writing the sample section of your manuscript.

- subid: Participant ID
- ntrial: Number of trials for the subject
- mcal: Mean calibration accuracy
- pcrit: Proportion of critical trials (see cleaning)
- pplink: Proportion of trials with blinks
- pout: Proportion of trials with outlier fixations
- nrun: Mean number of runs across trials
- nfix: Mean number of fixation across trials
- skip: Mean proportion of words that have been skipped during first-pass reading across trials
- sac: Mean (forward) saccade length
- refix: Mean proportion of words that have been refixated across trials
- reg: Mean proportion of words which have been regressed into across trials
- mfix: Mean fixation duration
- total: Mean total reading time across trials
- rate: Mean reading rate (words per minute) across trials


### Trial report 

- subid: Participant ID
- trialid: Position of trial in analysis
- trialnum: Position of trial in experiment
- itemid: Item ID
- cond: Condition (if applicable)
- trial: Name of trial (abbreviated)
- trial.nwords: Number of words in trial
- nblink: Number of blinks in trial
- nrun: Number of runs on trial
- nfix: Number of fixations on trial
- nout: Number of outlier fixations on trial
- sac: Mean (forward) sacade length
- skip: Proportion of words in the trial that have been skipped during first-pass reading
- refix: Proportion of words in the trial that have been refixated
- reg: Proportion of words which have been regressed into
- mfix: Mean fixation duration
- firstpass: First-pass reading time (summed gaze duration for all words in a trial)
- rereading: Re-reading time (total reading time minus first-pass reading time)
- total: Total reading time
- rate: Reading rate (words per minute)


### Sentence report

- subid: Participant ID
- trialid: Position of trial in analysis
- trialnum: Position of trial in experiment
- itemid: Item ID
- cond: Condition (if applicable)
- sentnum: Number of sentence in trial
- sent: Name of sentence (abbreviated)
- sent.nwords: Number of words in sentence
- blink: Whether blink occured during reading the sentence
- skip: Whether the sentence has been skipped
- nrun: Number of times the sentence has been read
- reread: Whether the sentence has been read more than one time
- reg.in: Whether a regression has been made into the sentence
- reg.out: Whether a regression has been made out of the sentence
- total.nfix: Number of fixations made on the sentence
- total.dur: Total sentence reading time
- rate: Reading rate (number of words per minute)
- gopast: Sum of all fixations durations from the time the sentence was entered until it was left to the right (regression path duration)
- gopast.sel: Sum of all fixations on the sentence from the time it was entered until it was left to the right (selective go-past time: regression path dur ation minus the time of the regression path)
-  firstrun.skip: Whether sentence has been skipped during first-pass reading
- firstrun.reg.in: Whether a regression has been made into the sentence during first-pass reading
- firstrun.reg.out: Whether a regression has been made out of the sentence during first-pass reading
- firstpass.nfix: Number of fixation made during first-pass reading
- firstpass.dur: First-pass reading time
- firstpass.forward.nfix: Number of first-pass forward fixations (landing on one of the upcoming words of a sentence)
- firstpass.forward.dur: Duration of forward fixations during first-pass reading
- firstpass.reread.nfix: Number of first-pass rereading fixations (landing one of the words of the sentence that have been read previously)
- firstpass.reread.dur: Duration of rereading fixations during first-pass reading
- lookback.nfix: Number of fixations made on the sentence after regressing into it from another sentence
- lookback.dur: Duration of lookback fixations on the sentence
- lookfrom.nfix: Number of rereading fixations on another sentence initiated from the sentence
- lookfrom.dur: Duration of lookfrom fixations on the sentence

The forward, rereading, look-back, and look-from measures are computed in similar way as in the SR "Getting Reading Measures" tool (https://www.sr-support.com/thread-350.html) which is based on the Eyelink Analysojia software (developed by the Turku Eye Labs).

### Interest area report

- subid: Participant ID
- trialid: Position of trial in analysis
- trialnum: Position of trial in experiment
- itemid: Item ID
- cond: Condition (if applicable)
- sentnum: Number of sentence within text
- ianum: Number of interest area (IA) within text
- ia: Text of IA
- blink: Variable indicating whether there was blink directly before, during, or directly after the IA was fixated
- skip: Variable indicating whether the IA was fixated in the trial
- nrun: Number of times the IA was reread within the trial ("reread" means that it was read again after it has been left to the left or right)
- reread: Variable indicating whether the IA was reread at least once during the trial
- nfix: Number of fixations on the IA during the whole trial
- refix: Variable indicating whether the IA has been refixated at least once during a trial
- reg.in: Variable indicating whether there was at least one regression into the IA 
- reg.out: Variable indicating whether there was at least one regression from the IA
- dur: Total time the IA was read during the trial in ms (total reading time)
- firstrun.skip: Variable indicating whether the IA was skipped during first-pass reading
- firstrun.nfix: Number of fixations made on the IA during first-pass reading
- firstrun.refix: Variable indicating whether the IA was refixated during first-pass reading
- firstrun.reg.in: Variable indicating whether there was a regression into the IA during first-pass reading
- firstrun.reg.out: Variable indicating whether there was a regression from the IA during first-pass reading
- firstrun.dur: Time the IA was read during first-pass reading (gaze duration)
- firstrun.gopast: Sum of all fixations durations from the time the IA was entered until it was left to the right (go-past time/regression path duration)
- firstrun.gopast.sel: Sum of all fixations on the IA from the time the IA was entered until it was left to the right (selective go-past time: go-past time minus the time of the regression path)
- firstfix.sac.in: Incoming saccade length (in letters) for the first fixation on the IA
- firstfix.sac.out: Outgoing saccade length (in letters) for the first fixation on the IA
- firstfix.launch: Launch site distance (incoming saccade length until the space before the IA)
- firstfix.land: Landing position (letter) of the first fixation on the IA
- firstfix.cland: Centered landing position of the first fixation on the IA (Vitu et al., 2001: landing position - ((wordlength + 1) / 2))
- firstfix.dur: Duration of the first fixation on the IA (first fixation duration)
- singlefix: Variable indicating whether the IA was fixated only once during first-pass reading
- singlefix.sac.in: Incoming saccade length (in letters) for the first fixation on the IA when it was fixated only once during first-pass reading
- singlefix.sac.out: Outgoing saccade length (in letters) for the first fixation on the IA when it was fixated only once during first-pass reading
- singlefix.launch: Launch site distance (incoming saccade length until the space before the IA) for the first fixation on the IA when it was fixated only once during first-pass reading
- singlefix.land: Landing position (letter) of the first fixation on the IA when it was read only once
- singlefix.cland: Centred landing position of the first fixation on the IA when it was read only once
- singlefix.dur: Duration of the first fixation on the IA when it was read only once

### Word report

- subid: Participant ID
- trialid: Position of trial in analysis
- trialnum: Position of trial in experiment
- itemid: Item ID
- cond: Condition (if applicable)
- sentnum: Number of sentence within text
- wordnum: Number of word within text
- word: Text of word
- blink: Variable indicating whether there was blink directly before, during, or directly after the word was fixated
- skip: Variable indicating whether the word was fixated in the trial
- nrun: Number of times the word was reread within the trial ("reread" means that it was read again after it has been left to the left or right)
- reread: Variable indicating whether the word was reread at least once during the trial
- nfix: Number of fixations on the word during the whole trial
- refix: Variable indicating whether the word has been refixated at least once during a trial
- reg.in: Variable indicating whether there was at least one regression into the word 
- reg.out: Variable indicating whether there was at least one regression from the word
- dur: Total time the word was read during the trial in ms (total reading time)
- firstrun.skip: Variable indicating whether the word was skipped during first-pass reading
- firstrun.nfix: Number of fixations made on the word during first-pass reading
- firstrun.refix: Variable indicating whether the word was refixated during first-pass reading
- firstrun.reg.in: Variable indicating whether there was a regression into the word during first-pass reading
- firstrun.reg.out: Variable indicating whether there was a regression from the word during first-pass reading
- firstrun.dur: Time the word was read during first-pass reading (gaze duration)
- firstrun.gopast: Sum of all fixations durations from the time the word was entered until it was left to the right (go-past time/regression path duration)
- firstrun.gopast.sel: Sum of all fixations on the word from the time it was entered until it was left to the right (selective go-past time: go-past time minus the time of the regression path)
- firstfix.sac.in: Incoming saccade length (in letters) for the first fixation on the word
- firstfix.sac.out: Outgoing saccade length (in letters) for the first fixation on the word
- firstfix.launch: Launch site distance (incoming saccade length until the space before the word)
- firstfix.land: Landing position (letter) of the first fixation on the word
- firstfix.cland: Centered landing position of the first fixation on the word (Vitu et al., 2001: landing position - ((wordlength + 1) / 2)
- firstfix.dur: Duration of the first fixation on the word (first fixation duration)
- singlefix: Variable indicating whether the word was fixated only once during first-pass reading
- singlefix.sac.in: Incoming saccade length (in letters) for the first fixation on the word when it was fixated only once during first-pass reading
- singlefix.sac.out: Outgoing saccade length (in letters) for the first fixation on the word when it was fixated only once during first-pass reading
- singlefix.launch: Launch site distance (incoming saccade length until the space before the word) for the first fixation on the word when it was fixated only once during first-pass reading
- singlefix.land: Landing position (letter) of the first fixation on the IA when it was fixated only once during first-pass reading
- singlefix.cland: Centred landing position of the first fixation on the IA when it was fixated only once during first-pass reading
- singlefix.dur: Duration of the first fixation on the IA when it was fixated only once during first-pass reading

### Fixation report

- subid: Participant ID
- trialid: Position of trial in analysis 
- trialnum: Position of trial in experiment
- itemid: Item ID
- cond: Condition (if applicable)
- fixid: Number of fixation in a trial
- start: Start time (in ms since start of the trial)
- stop: End time (in ms since start of the trial)
- xs: Raw x position (in pixel)
- ys: Raw y position (in pixel)
- xn: Corrected x position (in pixel), i.e. after drift correction and line assignment
- yn: Corrected y position (in pixel), i.e. after drift correction and line assignment
- ym: Mean y position (position of the line)
- dur: Duration 
- sac.in: Incoming saccade length (in letters)
- sac.out: Outgoing saccade length (in letters)
- type: Whether fixation is an outlier fixation ("out"), i.e. located outside the text area (see assign.outlier and assign.outlier.dist arguments)
- blink: Whether a blink occured directly before or after the fixation
- run: Number of run the fixation was assigned to (if applicable)
- linerun: Number of run on the line the fixation was assigned to (if applicable)
- line: Number of line the fixation was assigned to
- line.change: Difference between the line of the current and the last fixation
- line.let: Number of letter on line
- line.word: Number of word on line
- letternum: Number of letter in trial
- letter: Name of Letter
- wordnum: Number of word in trial
- word: Name of Word
- ianum: Number of IA in trial
- ia: Name of IA
- sentnum: Number of sentence in trial
- sent: Name of sent (abbreviated)
- sent.nwords: Number of words in sentence
- trial: Name trial (abbreviated)
- trial.nwords: Number of words in trial
- word.fix: Number of fixation on word
- word.run: Number of run the word the word was read
- word.runid: Number of the word run, the fixation belongs to
- word.run.fix: Number of fixation within the run
- word.firstskip: Whether word has been skipped during first-pass reading
- word.refix: Whether word has been refixated with current fixation
- word.launch: Launch site distance from the beginning of the word
- word.land: Landing position with word
- word.cland: Centered landing position (e.g., calculated from the center of the word)
- word.reg.out: Whether a regression was made out of the word
- word.reg.in: Whether a regression was made into the word
- ia.fix: Number of fixation on IA
- ia.run: Number of run the word the IA was read
- ia.runid: Number of the IA run, the fixation belongs to
- ia.run.fix: Number of fixation within the run
- ia.firstskip: Whether IA has been skipped during first-pass reading
- ia.refix: Whether IA has been refixated with current fixation
- ia.launch: Launch site distance from the beginning of the IA
- ia.land: Landing position with IA
- ia.cland: Centered landing position (e.g., calculated from the center of the IA)
- ia.reg.out: Whether a regression was made out of the IA
- ia.reg.in: Whether a regression was made into the IA
- sent.word: Number of word in sentence
- sent.fix: Number of fixation on sentence
- sent.run: Number of run on sentence
- sent.runid: Number of the sentence run, the fixation belongs to
- sent.firstskip: Whether the sentence has been skipped during first-pass reading
- sent.refix: Whether sentence was refixated wither current fixation
- sent.reg.out: Whether a regression was made out the sentence
- sent.reg.in: Whether a regression was made into the sentence

### Saccade report

- subid: Participant ID
- trialid: Position of trial in analysis
- trialnum: Position of trial in experiment
- itemid: Item ID
- cond: Condition (if applicable)
- sacid: Number of saccade in a trial
- msg: type of saccade (saccade vs. blink)
- xs: Raw start x position (in pixel)
- ys: Raw start y position (in pixel)
- xe: Raw end x position (in pixel)
- ye: Raw end y position (in pixel)
- xsn: Corrected start x position (in pixel), i.e. after drift correction and line assignment
- ysn: Corrected start y position (in pixel), i.e. after drift correction and line assignment
- xen: Corrected end x position (in pixel), i.e. after drift correction and line assignment
- yen: Corrected end y position (in pixel), i.e. after drift correction and line assignment
- start: Start time (in ms since start of the trial)
- stop: End time (in ms since start of the trial)
- dist.let: Saccade length (in letters)
- dur: Duration (in ms)

