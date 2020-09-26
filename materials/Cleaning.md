# Cleaning

This file describes the different cleaning options in popEye. There are three different aspects to this. The first is that popEye is not able to process a specific trial because there are insufficient data etc. The second is how the fixations in existing trials are cleaned. The third is how trials can be excluded in the analysis because the trial is in some way problematic (blink on target word, boundary not triggered, etc.).


## 1. Trial exclusion

These items are excluded by popEye during pre-processing:

- All practice trials (either by specifying the number `item.pracnum` (`0` by default) or some pattern that characterizes the item ID for practice trials (provided in `item.practice`, e.g. `^P` for removing all item IDs starting with P.
- Trials with item IDs that are not included in the stimulus file. That is, if you just want to analyze a subset of your trials (e.g., because you have run several experiments in the same session), you can simply exclude them by not including them in the stimulus file.
- Trials that are explicitly specified by `skip.item` (based on the item ID) or `skip.trial` (based on the trial ID).  
- Trials that have been repeated during data collection (e.g., in EyeTrack in order to recalibrate).
- If a trial comprises less than 3 fixations.
- If a trial comprises sample data with negative x and y values (indicating problems with the calibration).
- If more 25% of the sample data are missing (indicating severe tracker loss). 

The number of valid trials after pre-processing is reported as `ntrials` and the number of trials excluded as `nexc`in the `subject` output file. 

## 2. Fixation cleaning

### Cleaning

popEye uses a cleaning procedure that is very similar to the 4-step cleaning in SR's DataViewer. However, similar to EyeDoctor, the distance criterion in order to merge fixations is measured in letters, but not in degree visual angle. 

1. By default, fixations shorter than 80 ms are merged with longer fixations if they are within 1 letter distance. 

2. Here, fixations shorter than 40 ms are merged with longer fixation if they are within a distance of 3 letters. 

3. Check IAs 

4. Delete fixations outside interest areas.

Steps 3. + 4. are not executed by default. The reason is that fixations are deleted which can destroy the complete fixation sequence. I strongly recommend not to use it unless you have very good reasons to do so (and I wouldn't recommend it to use these steps in DataViewer too). 

### Outlier

Outlier are fixations that are 20% away from the text area (see here). These fixations are kept in the data file but explicitly flagged as outliers and not used during some steps of the analysis. If `clean.outlier = T` is used, outliers at the beginning and the end of the trial are discarded [check].


### Example text for manuscript.


## 3. Trial cleaning

Based on type of experiment.

Clean file.

Generally: `0` indicates no problem, `1`indicates a problem.

### Trial

These variables are computed for trials in all types of experiments. Variables related to this level all start with `trial` in the clean file. 

`trial.calibration`: This variable indicates whether there is valid calibration data for the trial. (In case you are wondering why this variable exists: Yes, it is apparently possible to collect data using an EyeLink without initial calibration!). A trial is also flagged if the calibration was very bad (> 1° degree). If this variable is flagged, the trial is considered `critical` (see below).

`trial.fix`: This variable indicates whether a minimum number of fixations was detected on the trial. This number is controlled by the `exclude.nfix` parameter in `popEye()` with 3 fixations as the default value. If this variable is flagged, the trial is considered `critical` (see below).

`trial.blink`: This variable indicates whether there was a blink during the trial (independent of its position within the trial). 

`trial.crit`: This is a summary variable indicating whether a trial is `critical` on the trial level. This variable can be used in higher-level cleaning functions (see below).


### Target 

### Boundary

### Fast Priming

Convenience function `CleanData()`
