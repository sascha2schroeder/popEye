## popEye

popEye is an integrated environment to analyze eye-tracking data from reading experiments. Its unique feature is that it allows to analyze data from different experimental paradigms (single sentence reading, boundary paradigm, fast priming paradigm, text reading), collected using different eye tracking devices (SR research, SMI, etc.) and software packages (EyeTrack, Experiment Builder, etc.) within the same workflow.

Please feel free to use, test, or contribute to the package. Any help is really appreciated! The package is experimental at present, so please be patient if you encounter problems when you use popEye for the first time. Please feel free to contact me (sascha2schroeder@gmail.com) or to open an an issue on the project's github site.


## What popEye will do for you

popEye's functionaltiy can be divided in two broad areas: data preprocessing 
and data analysis.

Regarding *data preprocessing* popEye aims at substituting software packages such
SR DataViewer or EyeDoctor. The idea is that you feed your raw eyetracking and a
simple stimulus file into it and popEye outputs an R file that can be used for later analysis. 
During this process, popEye will parse the raw data into saccades and fixations,
assign fixations to lines and letters, and computes reading measures for different levels of analysis. 
It is possible to define user-specific interest areas which are flexible and can be placed within words, 
between word groups or between paragraphs.

With regard to *data analysis* the popEye file still comprises all low-level data but also 
provides separate reports for different levels of analysis (fixation report, word-
and sentence-level reports, interest-area report, text-level report). In addition, 
it has customized plots for different kinds of experiments. This helps to visualize 
your data and to identify problematic trials. Finally, popEye automatically checks 
for problematic aspects in your data (whether there was a blink before the target word, 
whether a boundary change did not complete within a change saccade, etc.). These checks
can be used to remove such trials prior to the main analysis.



## When popEye will work

The central idea of popEye is to provide a hardware- and software-independent solution
for reading-related experiments. At present, however, popEye works only with specific
eye trackers and software packages used for stimulus presentation

- popEye currently works only for data collected using Eyelink eye trackers from SR research.
We will add support for eye trackers from other manufacturers (SMI, GazePoint, tobii) as soon as possible.

- popEye currently works only with experiments created with SR Research's 
Experiment Builder and the UMass EyeTrack package. I hope I will be able to include experiments created with PyGaze soon.

popEye supports both single- and multi-line experiments and works with 
proportional and disproportional fonts.

The following fonts are currently supported although not in all font sizes:

- Courier New 
- Consolas
- Times New Roman
- Symbol

(You have to understand that each font and font size combination has to be implemented separately).
There is no general problem why popEye shouldn't work with non-European (Chinese, Arabic, Hebrew, etc.) 
and unspaced scripts. However, it certainly needs some work to implement it.


## Installation

The easiest way to install popEye is via the devtools package

``` R
install_github('sascha2schroeder/popEye')
```
