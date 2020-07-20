Algorithms for the automated correction of vertical drift in eye tracking data
==============================================================================

This repo contains the analytical code and supporting data for a paper on vertical drift correction algorithms that we are currently preparing for submission to *Behavior Research Methods*. The top-level structure of the repo is:

- `algorithms/`: Matlab/Octave, Python, and R implementations of the drift correction algorithms.

- `code/`: Python code used to analyze the algorithms (using simulated and natural datasets).

- `data/`: Various unprocessed and processed data files explained in more detail below.

- `manuscript/`: LaTeX source and postscript figures for the manuscript.

- `visuals/`: Various visualizations and illustrations, including corrections of all 48 trials by all algorithms.


Data
----

The data is organized into four directories:

- `fixations/`: The original fixation sequences for the 48 sample trials are stored in `sample.json`. Each of the other JSON files corresponds to an algorithmic or human correction.

- `manual_corrections/`: Manual corrections performed by the two correctors plus the gold standard correction. Each file is a reading trial, and files are named by participant ID and passage ID. Each line in these files corresponds to a fixation (duration, x-coordinate, y-coordinate, and line assignment (0 is used to represent discard)).

- `passages/`: The text of each of the 12 passages.

- `simulations/`: Pickled numpy arrays which store the simulated performance results.


License
-------

Except where otherwise noted, this repository is licensed under a Creative Commons Attribution 4.0 license. You are free to share and adapt the material for any purpose, even commercially, as long as you give appropriate credit, provide a link to the license, and indicate if changes were made. See LICENSE.md for full details.
