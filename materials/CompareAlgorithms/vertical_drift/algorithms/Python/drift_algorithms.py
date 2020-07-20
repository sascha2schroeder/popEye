import numpy as np

######################################################################
# ATTACH
######################################################################

def attach(fixation_XY, line_Y):
	n = len(fixation_XY)
	for fixation_i in range(n):
		line_i = np.argmin(abs(line_Y - fixation_XY[fixation_i, 1]))
		fixation_XY[fixation_i, 1] = line_Y[line_i]
	return fixation_XY

######################################################################
# CHAIN
# 
# https://github.com/sascha2schroeder/popEye/
######################################################################

def chain(fixation_XY, line_Y, x_thresh=192, y_thresh=32):
	n = len(fixation_XY)
	dist_X = abs(np.diff(fixation_XY[:, 0]))
	dist_Y = abs(np.diff(fixation_XY[:, 1]))
	end_chain_indices = list(np.where(np.logical_or(dist_X > x_thresh, dist_Y > y_thresh))[0] + 1)
	end_chain_indices.append(n)
	start_of_chain = 0
	for end_of_chain in end_chain_indices:
		mean_y = np.mean(fixation_XY[start_of_chain:end_of_chain, 1])
		line_i = np.argmin(abs(line_Y - mean_y))
		fixation_XY[start_of_chain:end_of_chain, 1] = line_Y[line_i]
		start_of_chain = end_of_chain
	return fixation_XY

######################################################################
# CLUSTER
# 
# https://github.com/sascha2schroeder/popEye/
######################################################################

from sklearn.cluster import KMeans

def cluster(fixation_XY, line_Y):
	m = len(line_Y)
	fixation_Y = fixation_XY[:, 1].reshape(-1, 1)
	clusters = KMeans(m).fit_predict(fixation_Y)
	centers = [fixation_Y[clusters == i].mean() for i in range(m)]
	ordered_cluster_indices = np.argsort(centers)
	for fixation_i, cluster_i in enumerate(clusters):
		line_i = np.where(ordered_cluster_indices == cluster_i)[0][0]
		fixation_XY[fixation_i, 1] = line_Y[line_i]
	return fixation_XY

######################################################################
# COMPARE
#
# Lima Sanches, C., Kise, K., & Augereau, O. (2015). Eye gaze and text
#   line matching for reading analysis. In Adjunct proceedings of the
#   2015 ACM International Joint Conference on Pervasive and
#   Ubiquitous Computing and proceedings of the 2015 ACM International
#   Symposium on Wearable Computers (pp. 1227–1233). Association for
#   Computing Machinery.
#
# https://doi.org/10.1145/2800835.2807936
######################################################################

def compare(fixation_XY, word_XY, x_thresh=512, n_nearest_lines=3):
	line_Y = np.unique(word_XY[:, 1])
	n = len(fixation_XY)
	diff_X = np.diff(fixation_XY[:, 0])
	end_line_indices = list(np.where(diff_X < -x_thresh)[0] + 1)
	end_line_indices.append(n)
	start_of_line = 0
	for end_of_line in end_line_indices:
		gaze_line = fixation_XY[start_of_line:end_of_line]
		mean_y = np.mean(gaze_line[:, 1])
		lines_ordered_by_proximity = np.argsort(abs(line_Y - mean_y))
		nearest_line_I = lines_ordered_by_proximity[:n_nearest_lines]
		line_costs = np.zeros(n_nearest_lines)
		for candidate_i in range(n_nearest_lines):
			candidate_line_i = nearest_line_I[candidate_i]
			text_line = word_XY[word_XY[:, 1] == line_Y[candidate_line_i]]
			dtw_cost, _ = dynamic_time_warping(gaze_line[:, 0:1], text_line[:, 0:1])
			line_costs[candidate_i] = dtw_cost
		line_i = nearest_line_I[np.argmin(line_costs)]
		fixation_XY[start_of_line:end_of_line, 1] = line_Y[line_i]
		start_of_line = end_of_line
	return fixation_XY

######################################################################
# MERGE
#
# Špakov, O., Istance, H., Hyrskykari, A., Siirtola, H., & Räihä,
#   K.-J. (2019). Improving the performance of eye trackers with
#   limited spatial accuracy and low sampling rates for reading
#   analysis by heuristic fixation-to-word mapping. Behavior Research
#   Methods, 51(6), 2661–2687.
#
# https://doi.org/10.3758/s13428-018-1120-x
# https://github.com/uta-gasp/sgwm
######################################################################

phases = [{'min_i':3, 'min_j':3, 'no_constraints':False}, # Phase 1
          {'min_i':1, 'min_j':3, 'no_constraints':False}, # Phase 2
          {'min_i':1, 'min_j':1, 'no_constraints':False}, # Phase 3
          {'min_i':1, 'min_j':1, 'no_constraints':True}]  # Phase 4

def merge(fixation_XY, line_Y, y_thresh=32, g_thresh=0.1, e_thresh=20):
	n = len(fixation_XY)
	m = len(line_Y)
	diff_X = np.diff(fixation_XY[:, 0])
	dist_Y = abs(np.diff(fixation_XY[:, 1]))
	sequence_boundaries = list(np.where(np.logical_or(diff_X < 0, dist_Y > y_thresh))[0] + 1)
	sequence_starts = [0] + sequence_boundaries
	sequence_ends = sequence_boundaries + [n]
	sequences = [list(range(start, end)) for start, end in zip(sequence_starts, sequence_ends)]
	for phase in phases:
		while len(sequences) > m:
			best_merger = None
			best_error = np.inf
			for i in range(len(sequences)-1):
				if len(sequences[i]) < phase['min_i']:
					continue # first sequence too short, skip to next i
				for j in range(i+1, len(sequences)):
					if len(sequences[j]) < phase['min_j']:
						continue # second sequence too short, skip to next j
					candidate_XY = fixation_XY[sequences[i] + sequences[j]]
					gradient, intercept = np.polyfit(candidate_XY[:, 0], candidate_XY[:, 1], 1)
					residuals = candidate_XY[:, 1] - (gradient * candidate_XY[:, 0] + intercept)
					error = np.sqrt(sum(residuals**2) / len(candidate_XY))
					if phase['no_constraints'] or (abs(gradient) < g_thresh and error < e_thresh):
						if error < best_error:
							best_merger = (i, j)
							best_error = error
			if best_merger is None:
				break # no possible mergers, break while and move to next phase
			merge_i, merge_j = best_merger
			merged_sequence = sequences[merge_i] + sequences[merge_j]
			sequences.append(merged_sequence)
			del sequences[merge_j], sequences[merge_i]
	mean_Y = [fixation_XY[sequence, 1].mean() for sequence in sequences]
	ordered_sequence_indices = np.argsort(mean_Y)
	for line_i, sequence_i in enumerate(ordered_sequence_indices):
		fixation_XY[sequences[sequence_i], 1] = line_Y[line_i]
	return fixation_XY

######################################################################
# REGRESS
#
# Cohen, A. L. (2013). Software for the automatic correction of
#   recorded eye fixation locations in reading experiments. Behavior
#   Research Methods, 45(3), 679–683.
#
# https://doi.org/10.3758/s13428-012-0280-3
# https://blogs.umass.edu/rdcl/resources/
######################################################################

from scipy.optimize import minimize
from scipy.stats import norm

def regress(fixation_XY, line_Y, k_bounds=(-0.1, 0.1), o_bounds=(-50, 50), s_bounds=(1, 20)):
	n = len(fixation_XY)
	m = len(line_Y)

	def fit_lines(params, return_line_assignments=False):
		k = k_bounds[0] + (k_bounds[1] - k_bounds[0]) * norm.cdf(params[0])
		o = o_bounds[0] + (o_bounds[1] - o_bounds[0]) * norm.cdf(params[1])
		s = s_bounds[0] + (s_bounds[1] - s_bounds[0]) * norm.cdf(params[2])
		predicted_Y_from_slope = fixation_XY[:, 0] * k
		line_Y_plus_offset = line_Y + o
		density = np.zeros((n, m))
		for line_i in range(m):
			fit_Y = predicted_Y_from_slope + line_Y_plus_offset[line_i]
			density[:, line_i] = norm.logpdf(fixation_XY[:, 1], fit_Y, s)
		if return_line_assignments:
			return density.argmax(axis=1)
		return -sum(density.max(axis=1))

	best_fit = minimize(fit_lines, [0, 0, 0])
	line_assignments = fit_lines(best_fit.x, True)
	for fixation_i, line_i in enumerate(line_assignments):
		fixation_XY[fixation_i, 1] = line_Y[line_i]
	return fixation_XY

######################################################################
# SEGMENT
#
# Abdulin, E. R., & Komogortsev, O. V. (2015). Person verification via
#   eye movement-driven text reading model, In 2015 IEEE 7th
#   International Conference on Biometrics Theory, Applications and
#   Systems. IEEE.
#
# https://doi.org/10.1109/BTAS.2015.7358786
######################################################################

def segment(fixation_XY, line_Y):
	n = len(fixation_XY)
	m = len(line_Y)
	diff_X = np.diff(fixation_XY[:, 0])
	saccades_ordered_by_length = np.argsort(diff_X)
	line_change_indices = saccades_ordered_by_length[:m-1]
	current_line_i = 0
	for fixation_i in range(n):
		fixation_XY[fixation_i, 1] = line_Y[current_line_i]
		if fixation_i in line_change_indices:
			current_line_i += 1
	return fixation_XY

######################################################################
# SPLIT
######################################################################

def split(fixation_XY, line_Y):
	n = len(fixation_XY)
	diff_X = np.diff(fixation_XY[:, 0])
	clusters = KMeans(2).fit_predict(diff_X.reshape(-1, 1))
	centers = [diff_X[clusters == 0].mean(), diff_X[clusters == 1].mean()]
	sweep_marker = np.argmin(centers)
	end_line_indices = list(np.where(clusters == sweep_marker)[0] + 1)
	end_line_indices.append(n)
	start_of_line = 0
	for end_of_line in end_line_indices:
		mean_y = np.mean(fixation_XY[start_of_line:end_of_line, 1])
		line_i = np.argmin(abs(line_Y - mean_y))
		fixation_XY[start_of_line:end_of_line, 1] = line_Y[line_i]
		start_of_line = end_of_line
	return fixation_XY

######################################################################
# WARP
######################################################################

def warp(fixation_XY, word_XY):
	_, dtw_path = dynamic_time_warping(fixation_XY, word_XY)
	for fixation_i, words_mapped_to_fixation_i in enumerate(dtw_path):
		candidate_Y = word_XY[words_mapped_to_fixation_i, 1]
		fixation_XY[fixation_i, 1] = mode(candidate_Y)
	return fixation_XY

def mode(values):
	values = list(values)
	return max(set(values), key=values.count)

######################################################################
# Dynamic Time Warping adapted from https://github.com/talcs/simpledtw
# This is used by the COMPARE and WARP algorithms
######################################################################

def dynamic_time_warping(sequence1, sequence2):
	n1 = len(sequence1)
	n2 = len(sequence2)
	dtw_cost = np.zeros((n1+1, n2+1))
	dtw_cost[0, :] = np.inf
	dtw_cost[:, 0] = np.inf
	dtw_cost[0, 0] = 0
	for i in range(n1):
		for j in range(n2):
			this_cost = np.sqrt(sum((sequence1[i] - sequence2[j])**2))
			dtw_cost[i+1, j+1] = this_cost + min(dtw_cost[i, j+1], dtw_cost[i+1, j], dtw_cost[i, j])
	dtw_cost = dtw_cost[1:, 1:]
	dtw_path = [[] for _ in range(n1)]
	while i > 0 or j > 0:
		dtw_path[i].append(j)
		possible_moves = [np.inf, np.inf, np.inf]
		if i > 0 and j > 0:
			possible_moves[0] = dtw_cost[i-1, j-1]
		if i > 0:
			possible_moves[1] = dtw_cost[i-1, j]
		if j > 0:
			possible_moves[2] = dtw_cost[i, j-1]
		best_move = np.argmin(possible_moves)
		if best_move == 0:
			i -= 1
			j -= 1
		elif best_move == 1:
			i -= 1
		else:
			j -= 1
	dtw_path[0].append(0)
	return dtw_cost[-1, -1], dtw_path
