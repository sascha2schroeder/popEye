# Text Assignment

This document describes some arguments that are relevant for the text assignment phase within popEye and describes a possible workflow how to efficiently process larger studies. The main focus is on processing of longer texts (1 screen), but the same procedure is generally also relevant for sentence-level studies. In the following, I will first describe some analysis options (and corresponding arguments) that are relevant for text assignment. Next, I will describe a prototypical workflow that you can adopt for your eye-tracking analysis.

## Analysis options

Most of the text assignment within in popEye is done by the function `AssignStim` which is again supported by several other functions. This function is build in and you don't have to worry about its content. You simply have to specify the corresponding arguments when calling `popEye()` (or use the default values that should be appropriate for most settings). However, in order to fully understand what popEye is doing or to make some changes you can find all function in the `R`folder of the package. Here, I will walk you through the main steps of the analysis and describe the (implicit) analysis decisions and the corresponding arguments.

### 1. Drift correct

The first thing you have to decide if you want to use drift correction. Some eye tracking experiments - in particular, those created using SR's Experiment Builder software - comprise a so called `Drift Check` element. Here, a fixation point is presented before a trial and the task of the participant is fixate the point. Usually, the experimenter confirms that the participant is looking at the fixation point (e.g. by pressing the space bar). When the key is pressed, the deviation between the position of the fixation point and the actual gaze position is computed and the difference (in degree visual angle) is shown to the experimenter in order to inform him or her whether recalibration is needed. If the deviation is too high (1° by default, I believe), the trial will not be started. Thus, the main purpose of the element is provide feedback about the quality of the analysis. Next to this checking function, the drift values can be used to correct, i.e. move all fixations in a trial, during recording. Indeed, this was the default behavior for the Eyelink II where this element was called `Drift Correct`. For newer Eyelink eye-trackers, the correction is disabled by default. Even if the drift correct option is not enabled in your experiment, the difference values are recorded (and saved by `popEye` on the trial level) and can be used to correct fixation offline during the analysis. This can be done separately for the `x`and the `y` dimension. The corresponding arguments in the `popEye` function are

`assign.driftX = T`

and

`assign.driftY = T`

which will correct for vertical and horizontal drift respectively. The default is that drift is not corrected. However, if all fixations in a trial are shifted in a systematic way, it might make sense to enable this option (especially for the `y` axis). At the same time, however, drift correction can seriously affect the position of your fixations and should be used with caution (especially for drift on the `x` axis).

Please realize that is is not possible to use these arguments, if no `Drift Check` element has been used. This is typically the case in EyeTrack experiments. You'll get an error and the analysis is aborted. 

### 2. Outlier detection

The next thing you have to decide whether you want to flag fixations that are clearly not on the text field as outlier fixations. By default, outliers are defined as fixations that deviate from the text area by more than 20% of either the height or the width of the text area. This option is enabled by default and is particularly helpful for displays such as the following:

<img align="center" width="600" height="450" src="Pics/Trilingual.png?raw=true">

where a sentence was displayed at the top of the screen and some instruction which button to process at the bottom. Here, participants frequently looked at the instructions in the first trials. These are legitimate fixations, but they are clearly unrelated to the reading of the sentence. While it might be advisable to avoid such situations, the fixations can be flagged as outliers during the analysis. Importantly, the fixations are not deleted (as are fixations outside interest areas e.g. in SR's Data Viewer). Instead, they are kept in the data file, but ignored during the next steps of the analysis. For the computation of the eye movement measures outlying fixations are treated similar to blinks, i.e. they are not included in any reading time measures and fixations and interest areas are flagged if there was an outlying fixation before or after it.

Outlier detection can be enable by specifying 

`assign.outlier = T` 

in your `popEye` call (which is the default). Screening for outliers is typically helpful and not likely to have any negative consequences. If necessary, the outlier criterion (i.e., the distance to the text area, see above) can be set using 

`assign.outlierDist = 0.2`

with 20% as the default (which is a reasonable value in many situations).

### 3. Move fixations

The next decision to make is whether you want to "move" fixations. This is something you might want to do in trials, in which all fixations are clearly above or below their actual fixations. Usually, this is caused by head movements of the participants after calibration.

Sometimes all fixations are affected, such as here: 

<img align="center" width="800" height="450" src="Pics/tr4_3.png?raw=true">

In other cases, however, the problem might be related to a particular subarea of the screen (typically the upper or lower corners), such as here: 

<img align="center" width="800" height="450" src="Pics/tr5_8.png?raw=true">

In order to deal with such problems, a range of methods has been developed which are sometimes called "implicit calibration" or "offline recalibration" (e.g., Vadillo et al., 2015, Zhang & Hornof, 2011). These methods typically require some knowledge about potential fixation locations on the screen and work better if they are widely dispersed over the screen (e.g., in visual search tasks). In most reading studies, the only information available is that participants might have fixated the text area. Using this information, it might be possible to ameliorate the problem to some extent although trials with high degree of distortion likely have to be discarded.

In `popEye` two algorithms are available which develop the general idea to map the area of the fixation area onto the area of the text. The algorithms differ in which transformations are allowed (moving, stretching) and the kind of fit functions they use. All methods are experimental and only examples of possible algorithms. The methods can be selected using the `assign.moveMethod = "option"` argument with two fully implemented options at present:

`hit`: Here, the method tries to move the fixations in a way that a maximal number of fixations is located on each line of the text area. The algorithms does this using a computationally expensive, brute-force method that shifts fixations 50 pixels in each direction (separately on the `x` and the `y` dimension, see below). Using this method, fixations are moved as a whole, i.e. the relative position between them is not changed.

`area`: Here, the method tries to maximize the overlap between the fixation and the text area. In order to do this, a rectangle is fit to the fixation data which is then projected on the text area using projective transformations (see Strang, 2016, for an overview). In contrast to the `hit` method, the fixation area is not only moved but also stretched and rotated if necessary. However, transformations are restricted to be linear, i.e. non-linear distortions cannot be corrected. 

Both methods can be applied to the `x` and the `y` dimension separately by using `assign.moveX = T` and `assign.moveY = T`. The default is that fixations are not moved. You should be particularly cautious if you use the `area` method and carefully check your data. Generally, it is less problematic to move fixations vertically on the `y` axis.    

### 4. Line assignment

The most important decision is about the method to assign fixation to lines. Carr et al. (under review) have recently compared different line assignment algorithms using natural text reading data from children and adults. They provide an excellent and comprehensive description of the different line assignment methods. Here, I will summarize the main characteristics of the methods provided in `popEye` at present.

`atttach`: This method serves as a baseline method. Here, fixations are assigned to lines solely based on their vertical distance. As can be imagined, this method will have problems with curved lines (see Fig. 3).

`chain`: This method has been proposed by Schroeder (2019) and is based on similar approaches developed by Beymer and Russell (2005) and Hyrskykari (2006). In a first step, subsequent fixations are “chained” together if they lie within a spatial window defined by a horizontal and vertical distance parameter. In a second step, fixations are mapped to lines based on their absolute vertical distance. This approach is similar to the `attach` method, but the assignment is supposedly more stable as fixations are grouped into sequences before they are mapped to lines

`regress`: This method has been proposed by Cohen (2013) and is used in the `FixAlign` package. Here, regression lines are fit to all lines of the text simultaneously and fixations are assigned to individual lines based on their residual vertical distance. The regression lines are assumed to be linear and to have a constant slope. 

`merge`: This method has been proposed by Spakov et al. (2019). Similar to `chain`, fixations are first grouped into sequences which are then recursively merged with each other. The merging decision in each step is regression-based and controlled by different parameters.

A brief comparison of the performance of the different methods can be found [here](CompareAlgorithms/CompareAlgorithms.md). In summary, each method has its own strengths and weaknessess. However, the accuracy level of all algorithms on the trial level is quite low, i.e., there is a high percentage of trials in which at least some fixations are seriously misassigned and will need correction.

### 5. Interactive mode

In order to be able to assign fixations to lines in a semi-automatic fashion, there is an additional `interactive` method. Here, fixations are grouped into relatively small groups and pre-assigned to lines. For each group of fixations, you can either confirm or change the pre-selected line.

The different input options are:

1. Confirm pre-selection: "Enter"
2. Change line: Assign line and confirm with "Enter" (e.g. "1" + "2" + "Enter" for line 12)
3. Go back to last run: "b" (back) + "Enter"
4. Quit alignment: "Esc" (escape). Be careful, this will abort the complete `popEye` process.

## Recommended workflow

In general, I would recommend the following workflow:

1. Inspect your data and experiment which combination of assignment options (drift correct, move fixations, line assignment method) works best for your data.

2. Inspect the quality of the alignment using the `PlotAlign()` or `PlotAlignSubject()` functions.

3. Manually realign those trials that were misaligned. You can use the `select.subject` and `select.trial` arguments in the `popEye()` function to select just those participants and trials and save them in separate `RDS` files.

4. Merge the different `RDS` files using the `MergeExperiment()` or `SubstituteTrials()` functions (which have to implemented yet). 

