import numpy as np
import drift_algorithms

# 2D array representing the XY coordinates of the original fixations
fixation_XY = np.array([[395, 150], [479, 152], [619, 155], [670, 168], [726, 142], [912, 161], [1086, 176], [401, 212], [513, 230], [594, 228], [725, 229], [806, 231], [884, 216], [1000, 234], [1133, 225], [379, 270], [472, 273], [645, 310], [713, 289], [788, 288], [948, 286], [1072, 307], [378, 360], [496, 357], [634, 338]], dtype=int)

# 1D array representing the Y coordinates of the lines of text
line_Y = np.array([155, 219, 283, 347], dtype=int)

# Matrix representing the XY coordinate of the centers of the words (only used by compare and warp)
word_XY = np.array([[400, 155], [496, 155], [592, 155], [672, 155], [744, 155], [896, 155], [1080, 155], [392, 219], [496, 219], [592, 219], [704, 219], [808, 219], [896, 219], [1000, 219], [1120, 219], [384, 283], [496, 283], [640, 283], [720, 283], [824, 283], [952, 283], [1072, 283], [400, 347], [504, 347], [616, 347]], dtype=int)

print('Original fixation sequence')
print(fixation_XY)

attach_output = drift_algorithms.attach(fixation_XY.copy(), line_Y)
print('Output from the attach algorithm')
print(attach_output)

chain_output = drift_algorithms.chain(fixation_XY.copy(), line_Y)
print('Output from the chain algorithm')
print(chain_output)

cluster_output = drift_algorithms.cluster(fixation_XY.copy(), line_Y)
print('Output from the cluster algorithm')
print(cluster_output)

compare_output = drift_algorithms.compare(fixation_XY.copy(), word_XY)
print('Output from the compare algorithm')
print(compare_output)

merge_output = drift_algorithms.merge(fixation_XY.copy(), line_Y)
print('Output from the merge algorithm')
print(merge_output)

regress_output = drift_algorithms.regress(fixation_XY.copy(), line_Y)
print('Output from the regress algorithm')
print(regress_output)

segment_output = drift_algorithms.segment(fixation_XY.copy(), line_Y)
print('Output from the segment algorithm')
print(segment_output)

split_output = drift_algorithms.split(fixation_XY.copy(), line_Y)
print('Output from the split algorithm')
print(split_output)

warp_output = drift_algorithms.warp(fixation_XY.copy(), word_XY)
print('Output from the warp algorithm')
print(warp_output)
