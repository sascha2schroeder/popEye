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

### Word report

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




 
