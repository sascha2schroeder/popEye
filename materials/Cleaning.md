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

3. In the third step, fixations within the same interest area are merged with each other, if there are at least 3 fixations below the threshold and no fixation above the threshold. This step is not executed by default (see below). You can enable it by setting the `clean.stage3` parameter to true. The threshold can be controlled via `clean.stage3Dur`. The default value is 140 ms (as in DataViewer). Please be aware that only the detection, but not the merging step is implemented at present. If you use this option, fixations are flagged to be merged, which has to be done in a later step during your analysis.

4. In the fourth step, fixations below or above some threshold are deleted. Again, this step is not executed by default. You can enable it by setting the `clean.stage4` parameter `TRUE`. The minimum and maximum threshold can be controlled by setting `clean.stage4Min` and `clean.stage4Max` respectively (with 80 ms and 800 ms as default values as in DataViewer). 

Steps 3 and 4 are not executed by default. The reason is that fixations are deleted which can completely destroy the fixation sequence. This is particularly true for step 4. I strongly recommend not to use it unless you have very good reasons to do so (and I recommend not to use it in DataViewer too). 

## 3. Outlier detection

There are two situations where fixations are flagged as `outliers` in `popEye`:

- If `assign.outlier` is `TRUE`(which is the default), fixations 20% away from the text area are flagged as outliers (see [here](Assign.md) for a detailed description).
- Fixations might be flagged as outliers during line assignment. In particular, for `lineMethod` `assign` and `chain` fixations that are more than two line distances away from the top or the bottom of the text field are considered outliers. The distance can be controlled using the `assign.outlierY` parameter which specifies how many lines the fixations have to deviate from the text field (the default is 2).

If `clean.outlier = T` is used, outliers at the beginning and the end of the trial are discarded. All other outlying fixations are kept in the data file and flagged as outliers (by setting `type=out` in the fixation table). Again, this is to preserve the original sequence of fixations. However, outliers are not used during some steps of the later analysis analysis, but are treated similarly as blinks.


## 4. Trial cleaning

Based on type of experiment.

Clean file.

Generally: `0` indicates no problem, `1` indicates a problem.

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


<!-- Example text for manuscript -->


