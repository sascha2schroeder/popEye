# Cleaning

One of popEye's strengths is that it does a lot of things automatically during pre-processing that are difficult to do (and therefore usually not done) when using different software packages. One of these things is popEye's ability to inspect and clean your data. 

Many people use the output their pre-processing in their analysis without checking the quality of their data or excluding problematic trials. This is bad practice because eye-tracking data are inherently noisy and you should always be careful which data you include in your analysis. Don't be afraid that cleaning your data affords some decisions on your side and is therefore "subjective". Everything is explicitly included in your code and each step can easily be replicated and modified if need.    

This file describes the different cleaning options in popEye. There are four different aspects to this. The first is which trials popEye excludes during processing, e.g. because there are insufficient data etc. The second is how fixations within trials are cleaned. The third is how outlying fixations are defined and treated. And the fourth and last aspect is, how you can exclude trials after pre-processing prior to your analysis, because the trial is in some way problematic (target word with blinks, boundary not triggered, etc.). While the first three points are pretty generic and implemented in all software packages, the fourth point is quite unique and worth checking out. 

popEye is highly flexible, but also provides reasonable default settings. So if you don't change anything and just use the default values, you will usually do fine. However, if you explore the different options you will get cleaner data and thus improve your analysis. In addition, you will get a better impression of your data quality and maybe learn how to improve your experimental setup. So, even if it seems to be complicated at first (which it isn't), it is definitely worth the effort. 

## 1. Trial exclusion during pre-processing

These items are excluded by popEye during pre-processing:

- All practice trials (either by specifying the number `item.pracnum` (`0` by default) or some pattern that characterizes the item ID for practice trials (provided in `item.practice`, e.g. `^P` for removing all item IDs starting with P.
- Trials with item IDs that are not included in the stimulus file. That is, if you just want to analyze a subset of your trials (e.g., because you have run several experiments in the same session), you can simply exclude them by not including them in the stimulus file.
- Trials that are explicitly specified by `skip.item` (based on the item ID) or `skip.trial` (based on the trial ID).  
- Trials that have been repeated during data collection (e.g., in EyeTrack in order to recalibrate).
- If a trial comprises less than 3 valid fixations.
- If a trial comprises sample data with negative x and y values (indicating problems with the calibration).
- If more 25% of the sample data are missing (indicating severe tracker loss). 

The number of valid trials after pre-processing is reported as `ntrials` and the number of trials excluded as `nexc`in the `subject` output file. 

## 2. Fixation cleaning

popEye uses a cleaning procedure that is very similar to the 4-stage cleaning in SR's DataViewer. However, similar to EyeDoctor, the distance criterion in order to merge fixations is measured in letters, but not in degree visual angle. 

1. In the first step, fixations shorter than 80 ms are merged with longer fixations if they are within 1 letter distance by default. These are the same default values as in EyeDoctor. They can be adjusted using the `clean.stage1Dur`and `clean.stage1Dist` parameters when calling `popEye`.  

2. In the second step, fixations shorter than 40 ms are merged with longer fixation if they are within a distance of 3 letters by default. Again, these settings are identical to the default values from EyeDoctor. They can be adjusted using the `clean.stage2Dur`and `clean.stage2Dist` parameters when calling `popEye`.    

3. In the third step, fixations within the same interest area are merged with each other, if there are at least 3 fixations below the threshold and no fixations above the threshold. This step is not executed by default (see below). You can enable it by setting the `clean.stage3` parameter to true. The threshold can be controlled via `clean.stage3Dur`. The default value is 140 ms (as in DataViewer). Please be aware that only the detection, but not the merging step is implemented at present. If you use this option, fixations are flagged to be merged, which has to be done in a later step during your analysis.

4. In the fourth step, fixations below or above some threshold are deleted. Again, this step is not executed by default. You can enable it by setting the `clean.stage4` parameter `TRUE`. The minimum and maximum threshold can be controlled by setting `clean.stage4Min` and `clean.stage4Max` respectively (with 80 ms and 800 ms as default values as in DataViewer). 

Steps 3 and 4 are not executed by default. The reason is that fixations are deleted which can completely destroy the fixation sequence. This is particularly true for step 4. I strongly recommend not to use it unless you have very good reasons to do so (and I recommend not to use it in DataViewer too). 

## 3. Outlier detection

There are two situations where fixations are flagged as `outliers` in `popEye`:

- If `assign.outlier` is `TRUE`(which is the default), fixations 20% away from the text area are flagged as outliers (see [here](Assign.md) for a detailed description).
- Fixations might be flagged as outliers during line assignment. In particular, for `lineMethod` `assign` and `chain` fixations that are more than two line distances away from the top or the bottom of the text field are considered outliers. The distance can be controlled using the `assign.outlierY` parameter which specifies how many lines the fixations have to deviate from the text field (the default is 2). Similarly, fixations might not be assigned to lines using the `interactive` method, which are then regarded as outliers.

If `clean.outlier = T` is used, outliers at the beginning and the end of the trial are discarded. All other outlying fixations are kept in the data file and flagged as outliers (by setting `type=out` in the fixation table). Again, this is to preserve the original sequence of fixations. However, outliers are not used during some steps of the later analysis analysis, but are treated similarly as blinks.


## 4. Trial cleaning

One of the most powerful tools within popEye is that it automatically looks for problematic aspects in your data and records them so they can later be used to exclude trials prior to your main analysis. Typical patterns include whether a trial was read properly, whether there was a blink prior to the target word, or whether the boundary was triggered appropriately. What aspects are important obviously depends on the type of experiment you are running. So the output will vary based on what you specified as `type` in your `popEye`call.

During pre-processing popEye generates a `clean` file in the output slot of the RDS file that can be used during the analysis. In this file, there is one row for each trial in your experiment and each column represents a different aspect that popEye has checked. So structurally, the file is similar to the `trial` report. For each of the variables `0` indicates no problem, `1` indicates a problem. The variables are structured in different sections.

### General information

The first section of the `clean` file provides general information about each the trial including subject ID, trial ID, item ID etc. similar to the trial report. However, there is also information about the calibration method, the average quality of the last calibration before the trial was run, the maximum accuracy of this calibration, and the offset for each trial (combined in degree visual angle and separately in pixels for the x- and the y-dimension) if driftcheck was enabled in your experimen the maximum accuracy of this calibration, and the offset for each trial (combined in degree visual angle and separately in pixels for the x- and the y-dimension) if driftcheck was enabled in your experiment. 

<!-- Maybe use screen shots for the different sections? -->

### Trial

Variables in the next section pertain to the trial level and are computed for all types of experiments. Variables related to this level all start with `trial` in the clean file. 

`trial.calibration`: This variable indicates whether there is valid calibration data for the trial. (In case you are wondering why this variable exists: Yes, it is apparently possible to collect data using an EyeLink without initial calibration!). A trial is also flagged if the calibration was very bad (> 1° degree). If this variable is flagged, the trial is considered `critical` (see below).

`trial.fix`: This variable indicates whether a minimum number of fixations was detected on the trial. This number is controlled by the `exclude.nfix` parameter in `popEye` with 3 fixations as the default value. If this variable is flagged, the trial is considered `critical` (see below).

`trial.blink`: This variable indicates whether there was a blink during the trial (independent of its position within the trial). 

`trial.crit`: This is a summary variable indicating whether a trial is `critical` on the trial level. This variable can be used to exclude trials or in higher-level cleaning functions (see below).

### Target 

The next level of analysis is only done for experiments that have a target word (i.e., type `target` or higher) and are relevant if there is one word in your item, that is of particular interest to you. Variables in this section all start with `target`. In addition, there are variables related to processing of the target word in general, to the behavior of the eyes before reading the target word (`pre`) and after leaving it (`post`). 

`target.blink`: This variable indicates whether there is a blink directly involving the target word or right before or after it. If this variable is flagged, the trial is considered `critical`. 

`target.out`: This variable indicates whether there is an outlying fixation right before or after the target word. If yes, the trial is considered `critical`. 

`target.first`: This variable indicates whether the first fixation on the trial is on or a region after the target word, i.e. whether the trial has been not been read before reading the target word. If yes, the trial is considered `critical`.  

`target.pre.blink`: This variable indicates whether there is a blink (anywhere) before the target word has been fixated for the first time.

`target.pre.out`: This variable indicates whether there is an outlying fixation (anywhere) before the target word has been fixated for the first time. 

`target.pre.launch`: This variable indicates whether the launch fixation was not located on word n-1 or n-2. This variable can be used in order to exclude trials with very distant launch sites. 

`target.pre.refix`: This variable indicates whether the word of the pre-target fixation has been refixated. In this case the target word might have got more parafoveal preview than expected from its launch site. This is particularly true if the first fixation on the word was nearer to the target word.

`target.pre.reg`: This variable indicates whether a letter nearer to the target word has been visited before the launch saccade. In this case the target word might have got more parafoveal preview than expected from its launch site (see also `target.pre.refix`).

`target.post.fix`: This variable indicates whether there is a fixation on an interest area after the target word, i.e. whether reading of sentence has stopped before or on the target word. If this variable is flagged, the trial is considered `critical`. 

`target.post.reg`: This variable indicates whether the target word has been exited forwards or backwards, i.e. whether a regression has been launched from the target words. 

`target.crit`: This is a summary variable indicating whether a trial is `critical` on the target level. This variable can be used to exclude trials or in higher-level cleaning functions.

### Boundary

The next level of analysis pertains only to boundary change and fast priming experiments in which a display change occurs during reading the text (i.e., type `boundary` or higher). Variables in this section all start with `boundary`. 

`boundary.trigger`: This variable indicates whether the boundary has not been triggered. If yes, the trial is considered `critical`. 

`boundary.seq`: This variable provides the labels of the events right before or after the boundary change. There are two standard patterns: saccade-boundary-target-fixation (if the boundary change was completed before fixation started) and saccade-boundary-fixation-target (if the boundary change was completed during the target fixations). `1` indicates all non-standard sequences. If a non-standard pattern is observed, the trial is considered `critical`.

`boundary.blink`: This variable indicates whether there was a blink right before or after the boundary. If yes, the trial is considered `critical`. 

`boundary.out`: This variable indicates whether there was an outlying fixation right before or after the boundary. If yes, the trial is considered `critical`. 

`boundary.time`: This variable indicates whether the boundary was triggered more than 10 ms into the target fixation. If yes, the trial is considered `critical` (see Slattery et al., 2011).

`boundary.hook`: This indicates whether the boundary was triggered during a saccade, but the eye landed on the pre-target word (a so called J-hook). If yes, the trial is considered `critical`. 

`boundary.change.sac`: This variable indicates the duration of the change saccade. If the duration is longer than 80 ms, it is considered a blink (see `boundary.blink`) and, therefore, `critical`. 

`boundary.pre.time`: This variable provides the difference between the time of the onset of the change saccade and the time the boundary was triggered. 

`boundary.target.time`: This variable indicates the time between the boundary change and the onset of the target display.

`boundary.post.time`: This variable indicates the time between the target onset and the start of the first fixation of the target (values are negative if the target display was changed within the fixation; see `boundary.time`).

`boundary.target.fix`: This variable indicates the time of target fixation after the target has been displayed. If the onset of the target was within the fixation, this time is shorter than the complete fixation duration.

`boundary.crit`: This is a summary variable indicating whether a trial `critical`on the boundary level. This variable can be used to exclude trials or in higher-level cleaning functions.


### Fast Priming


### Convenience functions

Convenience function `CleanData()`


<!-- Example text for manuscript -->


