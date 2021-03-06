# A comparison of different line assignment methods using the MECO corpus

### Sascha Schroeder - July 20, 2020

# Introduction

Carr et al. (under review) have recently compared different line
assignment algorithms using natural text reading data from children and
adults. Here, I would like to replicate some of their analyses using
data from the MECO corpus (Kuperman et al., under review). I will report
the overall performance of the different algorithms, but also
demonstrate their strengths and weaknesses using concrete examples. All
necessary data and the corresponding Rmarkdown file including all
analyses can be found here:
<https://github.com/sascha2schroeder/popEye/tree/master/materials/CompareAlgorithms>

In the following, I will first briefly describe some relevant
characteristics of the MECO corpus and review the different line
assignment algorithms that have been used in the present analysis.

## The MECO corpus

MECO is a cross-linguistic, multi-site study investigating eye movements
while participants read texts in their native language (see Kuperman et
al., under review). At present, the corpus comprises data from 13
different languages (including Dutch, English, Finnish, German, Greek,
Hebrew, Italian, Korean, Norwegian, Russian, Spanish, and Turkish).
Here, data from the German sub-sample are used.

The reading materials consisted of 12 short expository texts (100-200
words, 8-12 sentences, 10-15 lines, all on 1 page) which provided
Wikipedia-style information. Five of the texts were translated across
all languages while the other texts were unique to each language in
order to avoid translation artifacts. Each text was followed by four
comprehension questions.

Texts were presented on a 25 in LCD monitor with a resolution of 1080 x
1920 px at a distance of ca. 60 cm. The font was 20 pt Consolas (in
which each letter covers an area of 15 x 24 px) using double line
spacing (i.e., 48 px). Compared with the setup of the study analyzed by
Carr et al. (Courier New, 20 pt, line spacing of 64 px), the effective
font size and the line spacing were thus substantially smaller. The
layout of first text (about the Roman god Janus) is shown in Fig. 1.

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/plot1-1.png" alt="*Fig. 1: Layout of the first text*"  />

<p class="caption">

*Fig. 1: Layout of the first text*

</p>

</div>

Data were collected using an EyeLink 1000+ in a desktop setup and
standard settings. Raw data were processed using the `popEye` package
(version 0.6.2). I will use data from six participants that were fairly
representative of the total sample. In order to create a gold standard
for the comparisons, the fixations of all 6 x 12 = 72 trials (ca. 17,000
fixations) were manually assigned to one of the lines in each text. The
data from the first trial of the first participant (`ger10`) are shown
in Fig. 2 (different colors represent different lines).

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/plot2-1.png" alt="*Fig. 2: Fixation data and manual line assignment for the first trial of participant `ger10`*"  />

<p class="caption">

*Fig. 2: Fixation data and manual line assignment for the first trial of
participant `ger10`*

</p>

</div>

The data exhibit several characteristics that are representative for
natural reading settings (e.g., the accuracy of the recorded fixation
positions varies across the screen, lines are curved, participants
reread extensively). Overall, however, `ger10` was a “good” participant,
i.e. calibration was fairly accurate and he read the texts in a more or
less orderly fashion. Data from another, more problematic participant
will be shown later.

## Line assignment algorithms

An excellent and comprehensive description of the different line
assignment methods used in this analysis can be found in Carr et
al. (under review). Here, I will merely summarize their main
characteristics.

`atttach`: This method serves as a baseline method. Here, fixations are
assigned to lines solely based on their vertical distance. As can be
imagined, this method will have problems with curved lines (see Fig. 2,
where some of the fixations from line 1 are located on line 2).

`chain`: This method has been proposed by Schroeder (2019) and is based
on similar approaches developed by Beymer and Russell (2005) and
Hyrskykari (2006). In a first step, subsequent fixations are “chained”
together if they lie within a spatial window defined by a horizontal and
vertical distance parameter (`thresh_x` and `thresh_y`). In a second
step, fixations are mapped to lines based on their absolute vertical
distance. This approach is similar to the `attach` method, but the
assignment is supposedly more stable as fixations are grouped into
sequences before they are mapped to lines

`cluster`: This approach has also been suggested by Schroeder (2019).
Here, k-means clustering is used in order to group fixations based on
their horizontal position. In the implementation by Carr et al. the
number of clusters extracted corresponds to the number of lines in a
text. Clusters are then assigned to lines based on their relative
vertical position. This is problematic if some of the lines in a text
have not been been read.

`compare`: This method is based on an algorithm proposed by Lima Sanches
et al. (2015). Here, fixations are first segmented into sequences by
detecting return sweeps, i.e. regressions that are longer than some
threshold value (32 letters by default). After that, the fixation
sequences are matched to lines based on the horizontal distance between
the fixations and the centers of the words in each line. Carr et al. use
dynamic time warping as a distance metric (which is explained below).
The major problem of this approach is that the distribution of words
within lines is very similar due to the grid-like nature of texts. As a
consequence, lines are easily confused.

`merge`: This method has been proposed by Spakov et al. (2019). Similar
to `chain`, fixations are first grouped into sequences which are then
recursively merged with each other. The merging decision in each step is
regression-based and controlled by different parameters.

`regress`: This method has been proposed by Cohen (2013) and is used in
the `FixAlign` package. Here, regression lines are fit to all lines of
the text simultaneously and fixations are assigned to individual lines
based on their residual vertical distance. The regression lines are
assumed to be linear and to have a constant slope (an assumption which
is typically not realistic, see Fig. 2).

`segment`: This approach has been proposed by Abdulin and Komogortsev
(2015). Similar to `compare`, the algorithm first detects return sweeps
by using the *m-1* longest regressions to segment fixation into
sequences (where *m* is the number of lines in a text). In a second
step, fixations are mapped onto lines based on their relative vertical
position. The algorithm assumes that the number of return sweeps
corresponds to the number of lines in a text and, as consequence, will
have problems when lines are skipped or reread.

`split`: This method has been proposed by Carr et al. (under review).
Similar to `compare` and `segment`, it is based on the idea to first
detect return sweeps. However, in contrast to the other two methods, it
uses k-means clustering of saccades to do this. As a consequence, more
(or less) fixation sequences than lines can be detected. In a second
step, these sequences are mapped to lines based on their absolute
vertical distance.

`warp`: This method has also been proposed by Carr et al. (under review)
and is based on dynamic time warping. Dynamic time warping is a distance
metric that is often used in computer science and is used to align
points in two time series even if they have different spatial and
temporal characteristics. In its application to the line assignment
problem, one of the time series is the fixation sequence and the second
is the order of the words in the text. The approach assumes that texts
are read from top to bottom and from left to right and will thus also
have problems with skipping and rereading behavior.

I used the algorithms as implemented in R by Carr et al. (see
<https://github.com/jwcarr/vertical_drift/algorithms/R/drift_algorithms.R>)
and also adopted their parameter settings (adjusted to the smaller font
size in MECO). I do not report any data for the `compare` method as this
algorithm glitched on most of the trials. Anyway, it was the worst
performing method in Carr et al. and is clearly not competitive.

# Fitting process

The following code was used to assign fixations to lines using the
different algorithms. The `popEye` package is needed to retrieve the
pre-processed data from `popEye`’s output file. The functions provided
by Carr et al. were retrieved from the corresponding GitHub repository.
They return the new y position of the fixations which were recoded into
their corresponding line numbers. Please note that the fitting process
can take some time as some of the algorithms (particularly, `merge` and
`warp`) are computationally expensive.

``` r
library(popEye)
source("drift_algorithms.R")

# initialize output data frame
out <- NULL

sub <- c("ger10", "ger11", "ger12", "ger13", "ger14", "ger15")

# subject loop
for (s in 1:length(sub)) {
  
  # read popEye data
  x <- readRDS(paste("Data/", sub[s], ".RDS", sep = ""))
 
  # trial loop 
  for (i in 1:12) {
    
    # retrieve fixations and stimulus information
    trial <- SelectSubjectTrial(x, sub[s], i)
    fix <- trial$fix[trial$fix$type != "out", ]
    stim <- trial$meta$stimmat
    
    # extract xy position of fixation and words and y position of lines
    fixation_XY <- fix[c("xn", "yn")]
    word_XY <- data.frame(cbind(
      tapply(stim$xm, stim$ianum, mean), 
      tapply(stim$ym, stim$ianum, mean)
      ))
    line_Y <- tapply(stim$ym, stim$line, max)
    
    # attach
    new_attach <- attach(fixation_XY, line_Y)
    fix$line_attach <- as.numeric(as.factor(new_attach$yn))

    # chain
    new_chain <- chain(fixation_XY, line_Y, 
                       x_thresh=180, 
                       y_thresh=30)
    fix$line_chain <- as.numeric(as.factor(new_chain$yn))
    
    # cluster
    new_cluster <- cluster(fixation_XY, line_Y)
    fix$line_cluster <- as.numeric(as.factor(new_cluster$yn))
    
    # cluster2
    new_cluster2 <- cluster2(fixation_XY, line_Y)
    fix$line_cluster2 <- as.numeric(as.factor(new_cluster2$yn))
    
    # merge
    new_merge <- merge(fixation_XY, line_Y, 
                       y_thresh=30, 
                       g_thresh=0.1, 
                       e_thresh=20)
    fix$line_merge <- as.numeric(as.factor(new_merge$yn))
    
    # regress
    new_regress <- regress(fixation_XY, line_Y, 
                           k_bounds=c(-0.1, 0.1), 
                           o_bounds=c(-50, 50), 
                           s_bounds=c(1, 20))
    fix$line_regress <- as.numeric(as.factor(new_regress$yn))
    
    # segment
    new_segment <- segment(fixation_XY, line_Y)
    fix$line_segment <- as.numeric(as.factor(new_segment$yn))
    
    # split
    new_split <- split(fixation_XY, line_Y)
    fix$line_split <- as.numeric(as.factor(new_split$yn))
    
    # warp
    new_warp <- warp(fixation_XY, word_XY)
    fix$line_warp <- as.numeric(as.factor(new_warp$yn))
    
    out <- rbind(out, fix)
    
  }
  
}
```

The output of this step is a matrix in which each line represents a
fixation and which comprises columns providing the assignments of the
gold standard and the eight assignment methods. The automatic
assignments were then evaluated against the gold standard, aggregated on
the trial level, and transformed into long format. The final data frame
thus provides the average accuracy for each participant-trial
combination and each algorithm.

# Analysis

Fig. 3 shows the overall accuracy of the eight methods (means and 95%
CIs) and the distribution of the average accuracies for all trials.

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/plot3-1.png" alt="*Fig 3.: Assignment accuracy of the different methods for all trials*"  />

<p class="caption">

*Fig 3.: Assignment accuracy of the different methods for all trials*

</p>

</div>

This plot shows several key observations (see also Tab. 1):

  - The baseline method `attach` performs surprisingly good and is
    difficult to beat.
  - Only three methods (`chain`, `regress`, and `warp`) show better
    performance than this baseline.
  - Two methods (`merge` and `split`) perform slightly worse than the
    baseline, but are still competitive.
  - The `cluster` method shows the worst performance of all methods. The
    performance of the `segment` method is also very bad.
  - Some methods (e.g., `attach` and `chain`) have a more consistent
    behavior, i.e. they perform similarly on all trials. Other methods,
    by contrast, show good performance on some trials, but perform
    poorly on others.

|         |     M |    SD | Median | \>.95 | \>.99 |
| ------- | ----: | ----: | -----: | ----: | ----: |
| attach  | 0.900 | 0.103 |  0.951 | 0.500 | 0.181 |
| chain   | 0.940 | 0.074 |  0.965 | 0.569 | 0.333 |
| cluster | 0.399 | 0.225 |  0.356 | 0.028 | 0.000 |
| merge   | 0.824 | 0.227 |  0.920 | 0.306 | 0.069 |
| regress | 0.904 | 0.114 |  0.962 | 0.542 | 0.208 |
| segment | 0.749 | 0.273 |  0.850 | 0.403 | 0.125 |
| split   | 0.854 | 0.228 |  0.940 | 0.458 | 0.125 |
| warp    | 0.920 | 0.109 |  0.960 | 0.528 | 0.139 |

*Tab. 1: Descriptive Statistics*

More detailed information about the performance of each method is
provided in Tab. 1. Looking at the medians, the overall accuracy level
was similar to the one reported by Carr et al. In addition, the absolute
values for some of the algorithms was replicated with surprising
precision. For example, Carr et al. report a median accuracy of 96.4%
for the `chain` method for adults while I observed a value of 96.5%
here. Similarly, the values for the `attach` method (94.8 vs. 95.1%),
the `regress` method (97.1 vs. 96.2%), and the `warp` method (97.9
vs. 96.0%) are also very similar. The `merge` and the `split` method
performed slightly worse in the present analysis than reported by Carr
et al. (92.0 vs. 97.6% and 94.0 vs. 96.8% respectively). The most
striking difference, however, pertains to the `cluster` method, which
performed very good in Carr et al. (97.3%), but extremely bad in the
present analysis (38.1%). I will come back to this discrepancy in the
Discussion section. Similarly, the `segment` method performed worse in
the present analysis (85.0 vs. 97.3%). Indeed, the median accuracy
observed here was similar to the level Carr et al. report for children
(81.3%). This indicates that this method might be more sensitive to
deviations from standard reading behavior.

However, from a practical perspective, average accuracy across trials is
not the most useful statistic. Instead, it is typically more important
to know whether you have to discard or reanalyze a trial or not. In
order to investigate this issue, Tab. 1 also reports the proportion of
trials which have an average accuracy above 95 and 99% which are the
trials that are most likely to be usable. All algorithms show a rather
modest performance here. For example, even for the better performing
methods (`chain`, `regress`, `warp`) only ca. 50% of the trials reach
the 95% threshold. However, these trials are still very likely to
contain seriously misaligned fixations (as will be shown below). If we
increase the accuracy level to 99%, only two methods yield a substantial
number of usable trials (`chain`: 33%, `regress`: 21%). Put differently,
between 2/3 and 4/5 of the trials are likely to include serious
assignment errors and either have to be discarded or reanalyzed.

Another important question is how consistently the different methods
perform for different participants. In order to investigate this, Fig. 4
shows the mean accuracy of the various methods for each of the six
participants.

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/bar-1.png" alt="*Fig. 4: Mean accuracy for the different participants for each algorithm respectively*"  />

<p class="caption">

*Fig. 4: Mean accuracy for the different participants for each algorithm
respectively*

</p>

</div>

As can be seen, some of the participants are generally more difficult to
align than others. In addition, the different algorithms all have their
strengths and weaknesses with regard to specific participants. `chain`
and `regress` have a rather similar profile with very good performance
for participants `ger10`, `ger11`, and `ger13`, but rather poor
performance for participants `ger12`, `ger14`, and `ger15`. By contrast,
`warp` has a rather different profile: It performs much better for
participants `ger12` and `ger14`, but surprisingly poor for participant
`ger10`. Thus, the different methods have relative advantages for
different reading patterns.

However, the *type* of errors the different methods make is also
important. Even a single error can seriously undermine later steps in
the analysis. For example, if a fixation on line 1 is erroneously
assigned to line 10, all words on lines 2-9 will be regarded as skipped
during first-pass reading. In order to analyze whether the various
methods make different kinds of errors, Fig. 5 displays the confusion
matrices of four more promising algorithms, i.e., `chain`, `regress`,
`merge`, and `warp`. The plots show quite clearly that the four methods
have characteristic confusion patterns. Again, `chain` and `regress`
behave similarly: Most confusions are local, i.e. incorrectly assigned
fixations are typically assigned to the next or the previous line. By
contrasts, the errors made by the `warp` method (and to a lesser extent
by the `merge` method) are qualitatively different: Although most
confusions are also local, these methods are characterized by a larger
proportion of global confusions, i.e. fixations that are assigned to
very distant lines. For example, of the errors made by the `warp` method
on line 1, 65% are local confusions (i.e., they were assigned to line 2
or 3), but 30% are global (i.e., they were assigned to lines 10-13).
Thus, the errors made by `warp` and `merge` are more likely to have
serious consequences downstream in the analysis.

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/mosaic-1.png" alt="*Fig. 5: Confusion matrices between the manual assignment and different automatic line assignment methods*"  />

<p class="caption">

*Fig. 5: Confusion matrices between the manual assignment and different
automatic line assignment methods*

</p>

</div>

## Examples

In a last step, I will look at specific examples which can provide some
insight about the advantages and disadvantages of the different methods.
A first example (Fig. 6) is trial 10 of participant `ger12` in which the
`warp` method performed better than `chain` method. This trial is
characterized by high curvature of the initial lines and minimal
rereading behavior.

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/gold1-1.png" alt="*Fig. 6: Manual line assignment for trial 10 of participant `ger12`*"  />

<p class="caption">

*Fig. 6: Manual line assignment for trial 10 of participant `ger12`*

</p>

</div>

Fig. 7 shows the fixations as assigned by the eight algorithms for this
trial. Deviations from the manual assignment are highlighted in red. As
can be seen, the `attach`, `chain`, and `regress` methods clearly have
huge problems with the curved lines in the upper left quadrant of the
screen. This behavior is expected because these algorithms assign
sequences based on their absolute position. Compared to the other two
methods, the `chain` method is better in dealing with the more local
deviations, but the method is not able to show its full potential here.
Apparently, the distance between the fixations in the middle of the
lines is too large in order to establish a single fixation sequence and
the left sequence is assigned to the following line. This problem could
potentially be ameliorated by increasing the `thresh_x` parameter (see
below). With regard to the other methods, the `cluster` method obviously
misaligns most of the fixation in the trial right from the beginning.
The `merge` method is doing very well, it only misses some transition
fixations which are ambiguous anyway. However, it also fails to assign
the final sequence to the last line, which is a more serious error. The
`segment`, `split`, and `warp` methods all make the same, single error
and fail to detect that the pre-final line is reread. Instead, these
fixations are assigned to the final line although it does not even have
any words at the corresponding x positions. Please note that none of the
analyses would constitute an acceptable analysis of the trial even
though `merge`, `segment`, `split`, and `warp` made fewer errors.

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/ex1-1.png" alt="*Fig. 7: Automatic line assigned for trial 10 of participant `ger12`*"  />

<p class="caption">

*Fig. 7: Automatic line assigned for trial 10 of participant `ger12`*

</p>

</div>

The second example (Fig. 8) is trial 3 of participant `ger10` in which
the `chain` method performed better than the `warp` method. This trial
is characterized by erratic initial fixations and a high amount of
rereading.

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/gold2-1.png" alt="*Fig. 8: Manual line assignment for trial 3 of participant `ger10`*"  />

<p class="caption">

*Fig. 8: Manual line assignment for trial 3 of participant `ger10`*

</p>

</div>

Fig. 9 shows the fixations as assigned by the eight algorithms for this
trial. Deviations from the manual assignment are again plotted in red.
Both `chain` and `regress` are nearly perfect and they miss only some of
the transition fixations. `attach` is also doing well, but makes two
errors at the end of the initial lines which are slightly curved
downwards. `merge` is performing reasonably well, but misaligns a
rereading sequence at the beginning of the second line. However, the
data are quite messy here and it is not clear whether the manual
assignment is correct. With regard to the other methods, `cluster` has
serious problems in the bottom half of the screen. `segment` and `warp`
have a huge amount of misalignment at the beginning of the trial which
are caused by extensive rereading. Interestingly, `split` is doing much
better here, but it misses some of the line transitions during rereading
which are initiated by upward- or rightward-movements. Overall, only the
`chain` and the `regress` analyses are usable, while `attach` and
`merge` make minor, and all other methods major errors which are clearly
unacceptable.

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/ex2-1.png" alt="*Fig. 9: Automatic line assigned for trial 3 of participant `ger10`*"  />

<p class="caption">

*Fig. 9: Automatic line assigned for trial 3 of participant `ger10`*

</p>

</div>

# Discussion

In sum, the results clearly demonstrate that there is no single “silver
bullet” method that is likely to perform well in all situations. All
approaches have their own strengths and weaknesses which are related to
the way how they define fixation sequences and how those are assigned to
lines. Generally, the effectiveness of all methods is highly
context-sensitive and strongly depends on the layout of the text on the
screen, the quality of the eye-tracking setup, and the reading behavior
of the participant.

Overall, there are clear similarities between the results reported here
and by Carr et al., but there are also some marked differences. The most
obvious discrepancy pertains to the `cluster` method which performed
very well in Carr et al., but was by far the worst method in the present
analysis. This difference might be related to the way this method has
been implemented by Carr et al. In order to test this, I reran the
analysis using the `popEye` version of the `cluster` method. The results
of this analysis are provided in the first row of Tab. 2 for the two
example items, all items of participants `ger10` and `ger12`, and for
all participants. The corresponding results from the Carr et al. version
are reported in the second row.

|                | ger12:10 | ger12 | ger10:3 | ger10 | overall |
| -------------- | -------: | ----: | ------: | ----: | ------: |
| cluster popEye |    0.368 | 0.375 |   0.227 | 0.382 |   0.382 |
| cluster Carr   |    0.363 | 0.355 |   0.049 | 0.461 |   0.407 |
| merge popEye   |    0.930 | 0.964 |   0.996 | 0.995 |   0.964 |
| merge Carr     |    0.950 | 0.774 |   0.969 | 0.974 |   0.824 |
| chain popEye   |    0.930 | 0.921 |   1.000 | 0.910 |   0.952 |
| chain Carr     |    0.766 | 0.843 |   0.991 | 0.990 |   0.940 |

*Tab. 2: Mean accuracy for alternative implementations of the cluster,
chain, and merge algorithms*

For some trials (e.g., trial 3 of participant `ger10`), the `popEye`
implementation did indeed increase accuracy. Overall, however, the
results of the two versions are very similar, with a slight overall
advantage for the Carr et al. implementation. So what else can explain
the differences? One potential answer is that the `cluster` method is
especially sensitive to the differences between the setups of the two
studies (layout, calibration). Another possibility is that there might
be differences in the way the k-means method is implemented in `R`
(which was used here) and in `Python` (which was used by Carr et al.).

In addition, there were other, more subtle differences between the two
analyses. In particular, the accuracy of the `segment` method was
substantially lower in the present analysis than reported by Carr et
al. Presumably, this discrepancy is related to the difference in the
amount of rereading between the two studies, which is particularly
problematic for the `segment` method. In addition, compared to Carr et
al., the `warp` method performed slightly worse while the `chain` method
performed somewhat better. Given that the two algorithms were developed
and optimized with different use cases in mind, one important
implication is that new algorithms should be evaluated using
heterogeneous, multi-site benchmark data, which is the standard
procedure in the machine learning domain.

It would certainly be possible to improve the performance of some of the
algorithms by changing some aspects of their implementation or by using
different parameters. For example, `popEye` (as of version 0.7) provides
a slightly different implementation of the `merge` method in which some
of the merging steps have been optimized. I reran the analysis using
this new version. The results are provided in row 4 of Tab. 2 (with the
corresponding values from the Carr et al. version in row 5). Results
show that this version of algorithm does indeed boost overall accuracy
making it one of the most effective methods (at least for the MECO
data).

Similarly, it might be possible to increase the performance of the
`chain` method by using higher values for the `thresh_x` parameter. This
would allow to build up longer and more stable fixation sequences which
is particularly helpful for participants such as `ger12`. Again, I reran
the analysis and increased both the `thresh_x` (to 20 letters as is the
default in `popEye`) and the `thresh_y` parameter (to 48 px). The
results are provided in row 5 of Tab. 2 (with the corresponding values
for the Carr et al. parameters in row 6). They indicate that increasing
the thresholds does indeed improve the performance of this method for
participant `ger12`. At the same time, however, this seems to decrease
the flexibility of the algorithm to deal with local rereading sequences
initiated by short upward saccades which are characteristic of
participant `ger10`. As a consequence, the overall increase in accuracy
is rather modest. Such trade-off relationships between different kinds
of errors is characteristic for algorithms when they are applied to
heterogeneous data and no single strategy is optimal.

Given that the various methods all have complementary strengths and
weaknesses, it might be an promising idea to combine the results of
different approaches in order to create some more stable
“meta”-alignment. However, it is not immediately clear how this
could be done. One possibility would be to fit several algorithms
simultaneously and establish some kind of “voting” system. Another
possibility would be to use the methods sequentially. For example, it
would be possible to use the converging assignments of two different,
complimentary methods in order to establish a preliminary assignment
grid. In a second step, these anchoring fixations could be used to
disambiguate the remaining fixations. Indeed, the present data indicate
that such a procedure might be a reasonable starting point as the
`chain` and `warp` methods agree on ca. 87% of all fixations and the
error rate within this intersection is extremely low (ca. 0.2%).

Finally, I would like to remind everybody that the accuracy level on the
trial-level (which is the most important output criterion) is very low
for all algorithms. It would be naive to expect that only using an
automatic alignment method will allow to make valid inferences about
participants’ reading behavior. Eye-tracking researchers still have to
be prepared to invest a substantial amount of effort into the line
assignment process. All what the different software solutions can do at
the moment is to ease this process by a) reducing the amount of trials
that have to be analyzed manually and b) providing assistance during the
assignment process.

## Edit (July 25, 2020)

Jon Carr (personal communication, July 25, 2020) has confirmed that the
discrepancy between the two analyses regarding the `cluster` method is
due to the (more conservative) way the k-means method is implemented in
`R`. If the control parameters of the algorithm are set to
`max.iter=300, nstart=100` the results are very similar to the `Python`
implementation in their data. I also repeated the analysis using the new
parameter settings (`cluster2`, see Fig. 4b and Tab. 1b below):

<div class="figure" style="text-align: center">

<img src="CompareAlgorithms_files/figure-gfm/plot3b-1.png" alt="*Fig 3b.: Assignment accuracy of the different methods for all trials (including the new cluster method)*"  />

<p class="caption">

*Fig 3b.: Assignment accuracy of the different methods for all trials
(including the new cluster method)*

</p>

</div>

|          |     M |    SD | Median | \>.95 | \>.99 |
| -------- | ----: | ----: | -----: | ----: | ----: |
| attach   | 0.900 | 0.103 |  0.951 | 0.500 | 0.181 |
| chain    | 0.940 | 0.074 |  0.965 | 0.569 | 0.333 |
| cluster  | 0.399 | 0.225 |  0.356 | 0.028 | 0.000 |
| cluster2 | 0.880 | 0.143 |  0.958 | 0.528 | 0.236 |
| merge    | 0.824 | 0.227 |  0.920 | 0.306 | 0.069 |
| regress  | 0.904 | 0.114 |  0.962 | 0.542 | 0.208 |
| segment  | 0.749 | 0.273 |  0.850 | 0.403 | 0.125 |
| split    | 0.854 | 0.228 |  0.940 | 0.458 | 0.125 |
| warp     | 0.920 | 0.109 |  0.960 | 0.528 | 0.139 |

*Tab. 1b: Descriptive Statistics (including the new cluster method)*

Results show that the performance of the the algorithm did indeed change
dramatically. Its overall performance was similar to the `regress`
method with a median accuracy of .958. More importantly, on the
trial-level (i.e., accuracy \> .99) the performance of the method was
relatively good with ca. 24% of the trials analyzed correctly. Overall,
the `cluster` method with the new parameter settings performed on a
similar level as reported by Carr et al. (under review) and is generally
competitive. However, for a substantial number of trials, the algorithm
did not converge within the specified 300 iterations and presumably
provided sub-optimal results. As a consequence, the accuracy
distribution is more skewed than for other algorithms. While this
problem might be solved by increasing the number of iterations, it
indicates that the `cluster` method might have special problems with a
certain sub-type of trials.
