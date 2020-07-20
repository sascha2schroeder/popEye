source('drift_algorithms.R')

# Matrix representing the XY coordinates of the original fixations
fixation_XY <- matrix(c(395, 150, 479, 152, 619, 155, 670, 168, 726, 142, 912, 161, 1086, 176, 401, 212, 513, 230, 594, 228, 725, 229, 806, 231, 884, 216, 1000, 234, 1133, 225, 379, 270, 472, 273, 645, 310, 713, 289, 788, 288, 948, 286, 1072, 307, 378, 360, 496, 357, 634, 338), ncol=2, byrow=TRUE)

# Vector representing the Y coordinates of the lines of text
line_Y <- c(155, 219, 283, 347)

# Matrix representing the XY coordinate of the centers of the words (only used by compare and warp)
word_XY <- matrix(c(400, 155, 496, 155, 592, 155, 672, 155, 744, 155, 896, 155, 1080, 155, 392, 219, 496, 219, 592, 219, 704, 219, 808, 219, 896, 219, 1000, 219, 1120, 219, 384, 283, 496, 283, 640, 283, 720, 283, 824, 283, 952, 283, 1072, 283, 400, 347, 504, 347, 616, 347), ncol=2, byrow=TRUE)

print('Original fixation sequence')
print(fixation_XY)

attach_output <- attach(fixation_XY, line_Y)
print('Output from the attach algorithm')
print(attach_output)

chain_output <- chain(fixation_XY, line_Y)
print('Output from the chain algorithm')
print(chain_output)

cluster_output <- cluster(fixation_XY, line_Y)
print('Output from the cluster algorithm')
print(cluster_output)

compare_output <- compare(fixation_XY, word_XY)
print('Output from the compare algorithm')
print(compare_output)

merge_output <- merge(fixation_XY, line_Y)
print('Output from the merge algorithm')
print(merge_output)

regress_output <- regress(fixation_XY, line_Y)
print('Output from the regress algorithm')
print(regress_output)

segment_output <- segment(fixation_XY, line_Y)
print('Output from the segment algorithm')
print(segment_output)

split_output <- split(fixation_XY, line_Y)
print('Output from the split algorithm')
print(split_output)

warp_output <- warp(fixation_XY, word_XY)
print('Output from the warp algorithm')
print(warp_output)
