# Output files and eye movement measures

At the end of the preprocessing process, an RDS file is created. You can read in using `readRDS()`, e.g.

`myexp <- readRDS("somePath/someFolder/expFile.RDS")`

In this file, the raw data (x and y position for each time stamp, messages, stimulus information, parsed gaze events, etc.) are available. However, most of this information will typically be used only internally by popEye (e.g., for creating plots etc.) although users can retrieve it to do more advanced analysis. Most users will presumably want to work with data frames in which eye movement events have already been aggregated. This is similar to the reports provided SR's Data Viewer or the different output files from EyeDoctor. popEye provides such reports on different levels (participant, trial, interest area, etc.) and in each report different eye movement measures are provided that are specific to the level of analysis.  

In the following, I will describe the different reports and the measures that are provided within them. After that I will briefly explain how you can extract the raw data from the RDS file in case you are interested in them.

## Reports

All reports are stored in a specific slot of the RDS file which is called `reports`. Which reports are generated depends on the type of experiment that you have been running and that you specified when running `popEye()` (e.g., `type = "sentence"`). For example, text and sentence level variables are only provided if the type of the experiment is `text`. You can see which types are available for your experiment by using 

`str(myexp$reports, max.level = 1)` 

In the following, I will go through all reports from the highest to the lowest level.

### Subject report

This data frame reports summary data on the level of the individual participant. It is available for every type of experiment and probably most useful when you want to check your data and you have decide whether some participants should be excluded (e.g., because eye tracking quality was very low or they show strange reading behavior). In addition, it will help you when you are writing the sample section of your manuscript.

You can extract the subject report e.g. using `sub <- myexp$reports$subject` and than work with `sub` as with any other `R` data frame. 

### Trial report 

<img align="center" width="600" height="450" src="Pics/Trilingual.png?raw=true">

### Text report

### Sentence report

### Interest area report

- subid: Participant ID
- trialid: Position of trial in experiment
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
- firstfix.cland: Centered landing position of the first fixation on the IA (Vitu et al., 2001: (landing position - (wordlength + 1)) / 2)
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
- trialid: Position of trial in experiment
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
- firstfix.cland: Centered landing position of the first fixation on the word (Vitu et al., 2001: (landing position - (wordlength + 1)) / 2)
- firstfix.dur: Duration of the first fixation on the word (first fixation duration)
- singlefix: Variable indicating whether the word was fixated only once during first-pass reading
- singlefix.sac.in: Incoming saccade length (in letters) for the first fixation on the word when it was fixated only once during first-pass reading
- singlefix.sac.out: Outgoing saccade length (in letters) for the first fixation on the word when it was fixated only once during first-pass reading
- singlefix.launch: Launch site distance (incoming saccade length until the space before the word) for the first fixation on the word when it was fixated only once during first-pass reading
- singlefix.land: Landing position (letter) of the first fixation on the IA when it was fixated only once during first-pass reading
- singlefix.cland: Centred landing position of the first fixation on the IA when it was fixated only once during first-pass reading
- singlefix.dur: Duration of the first fixation on the IA when it was fixated only once during first-pass reading

### Fixation report

### Saccade report

### Cleaning report

## Raw data

### Experiment level

Setup 

Subject slots

### Subject level

Header

Trial slots

### Trial level

Meta 

XY

(VXY)

Parse

Fix

Sac

All

Clean





