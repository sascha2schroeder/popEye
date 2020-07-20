'''
If you are looking for the drift algorithms, please check the
"algorithms" directory at the top level of this repo, which contains
easier to read versions written in Python, Matlab, and R. This file
contains versions of the algorithms that are adapted to our evaluation
and visualization pipelines.
'''

import numpy as np
from sklearn.cluster import KMeans
from scipy.optimize import minimize
from scipy.stats import norm

def correct_drift(method, fixation_XY, passage, return_solution=False, **params):
	fixation_XY = fixation_XY.copy()
	if method in ['attach', 'chain', 'cluster', 'merge', 'regress', 'segment', 'split']:
		second_argument = passage.line_positions
	elif method in ['compare', 'warp']:
		second_argument = passage.word_centers()
	else:
		raise ValueError('Invalid method')
	function = globals()[method]
	return function(fixation_XY, second_argument, return_solution=return_solution, **params)

def attach(fixation_XY, line_Y, return_solution=False):
	n = len(fixation_XY)
	for fixation_i in range(n):
		line_i = np.argmin(abs(line_Y - fixation_XY[fixation_i, 1]))
		fixation_XY[fixation_i, 1] = line_Y[line_i]
	if return_solution:
		return fixation_XY, None
	return fixation_XY

def chain(fixation_XY, line_Y, x_thresh=192, y_thresh=32, return_solution=False):
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
	if return_solution:
		return fixation_XY, end_chain_indices
	return fixation_XY

def cluster(fixation_XY, line_Y, return_solution=False):
	m = len(line_Y)
	fixation_Y = fixation_XY[:, 1].reshape(-1, 1)
	clusters = KMeans(m).fit_predict(fixation_Y)
	centers = [fixation_Y[clusters == i].mean() for i in range(m)]
	ordered_cluster_indices = np.argsort(centers)
	for fixation_i, cluster_i in enumerate(clusters):
		line_i = np.where(ordered_cluster_indices == cluster_i)[0][0]
		fixation_XY[fixation_i, 1] = line_Y[line_i]
	if return_solution:
		return fixation_XY, (clusters, list(ordered_cluster_indices))
	return fixation_XY

def compare(fixation_XY, word_XY, x_thresh=512, n_nearest_lines=3, return_solution=False):
	line_Y = np.unique(word_XY[:, 1])
	n = len(fixation_XY)
	diff_X = np.diff(fixation_XY[:, 0])
	end_line_indices = list(np.where(diff_X < -x_thresh)[0] + 1)
	end_line_indices.append(n)
	solution = []
	start_of_line = 0
	for end_of_line in end_line_indices:
		gaze_line = fixation_XY[start_of_line:end_of_line]
		mean_y = np.mean(gaze_line[:, 1])
		lines_ordered_by_proximity = np.argsort(abs(line_Y - mean_y))
		nearest_line_I = lines_ordered_by_proximity[:n_nearest_lines]
		line_costs = np.zeros(n_nearest_lines)
		text_lines = []
		warping_paths = []
		for candidate_i in range(n_nearest_lines):
			candidate_line_i = nearest_line_I[candidate_i]
			text_line = word_XY[word_XY[:, 1] == line_Y[candidate_line_i]]
			dtw_cost, warping_path = dynamic_time_warping(gaze_line[:, 0:1], text_line[:, 0:1])
			line_costs[candidate_i] = dtw_cost
			text_lines.append(text_line)
			warping_paths.append(warping_path)
		line_i = nearest_line_I[np.argmin(line_costs)]
		solution.append((gaze_line.copy(), text_lines, warping_paths, line_costs))
		fixation_XY[start_of_line:end_of_line, 1] = line_Y[line_i]
		start_of_line = end_of_line
	if return_solution:
		return fixation_XY, solution
	return fixation_XY

phases = [{'min_i':3, 'min_j':3, 'no_constraints':False},
          {'min_i':1, 'min_j':3, 'no_constraints':False},
          {'min_i':1, 'min_j':1, 'no_constraints':False},
          {'min_i':1, 'min_j':1, 'no_constraints':True}]

def merge(fixation_XY, line_Y, y_thresh=32, g_thresh=0.1, e_thresh=20, return_solution=False):
	n = len(fixation_XY)
	m = len(line_Y)
	diff_X = np.diff(fixation_XY[:, 0])
	dist_Y = abs(np.diff(fixation_XY[:, 1]))
	sequence_boundaries = list(np.where(np.logical_or(diff_X < 0, dist_Y > y_thresh))[0] + 1)
	sequences = [list(range(start, end)) for start, end in zip([0]+sequence_boundaries, sequence_boundaries+[n])]
	for phase in phases:
		while len(sequences) > m:
			best_merger = None
			best_error = np.inf
			for i in range(len(sequences)):
				if len(sequences[i]) < phase['min_i']:
					continue
				for j in range(i+1, len(sequences)):
					if len(sequences[j]) < phase['min_j']:
						continue
					candidate_XY = fixation_XY[sequences[i] + sequences[j]]
					gradient, intercept = np.polyfit(candidate_XY[:, 0], candidate_XY[:, 1], 1)
					residuals = candidate_XY[:, 1] - (gradient * candidate_XY[:, 0] + intercept)
					error = np.sqrt(sum(residuals**2) / len(candidate_XY))
					if phase['no_constraints'] or (abs(gradient) < g_thresh and error < e_thresh):
						if error < best_error:
							best_merger = (i, j)
							best_error = error
			if not best_merger:
				break
			merge_i, merge_j = best_merger
			merged_sequence = sequences[merge_i] + sequences[merge_j]
			sequences.append(merged_sequence)
			del sequences[merge_j], sequences[merge_i]
	mean_Y = [fixation_XY[sequence, 1].mean() for sequence in sequences]
	ordered_sequence_indices = np.argsort(mean_Y)
	for line_i, sequence_i in enumerate(ordered_sequence_indices):
		fixation_XY[sequences[sequence_i], 1] = line_Y[line_i]
	if return_solution:
		return fixation_XY, {line_i:sequences[sequence_i] for line_i, sequence_i in enumerate(ordered_sequence_indices)}
	return fixation_XY

def regress(fixation_XY, line_Y, k_bounds=(-0.1, 0.1), o_bounds=(-50, 50), s_bounds=(1, 20), return_solution=False):
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

	best_fit = minimize(fit_lines, [0, 0, 0], method='powell')
	line_assignments = fit_lines(best_fit.x, True)
	for fixation_i, line_i in enumerate(line_assignments):
		fixation_XY[fixation_i, 1] = line_Y[line_i]
	if return_solution:
		k = k_bounds[0] + (k_bounds[1] - k_bounds[0]) * norm.cdf(best_fit.x[0])
		o = o_bounds[0] + (o_bounds[1] - o_bounds[0]) * norm.cdf(best_fit.x[1])
		s = s_bounds[0] + (s_bounds[1] - s_bounds[0]) * norm.cdf(best_fit.x[2])
		return fixation_XY, ((k, o, s), line_assignments)
	return fixation_XY

def segment(fixation_XY, line_Y, return_solution=False):
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
	if return_solution:
		return fixation_XY, line_change_indices
	return fixation_XY

def split(fixation_XY, line_Y, return_solution=False):
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
	if return_solution:
		return fixation_XY, end_line_indices
	return fixation_XY

def warp(fixation_XY, word_XY, return_solution=False):
	_, warping_path = dynamic_time_warping(fixation_XY, word_XY)
	for fixation_i, words_mapped_to_fixation_i in enumerate(warping_path):
		candidate_Y = word_XY[words_mapped_to_fixation_i, 1]
		fixation_XY[fixation_i, 1] = mode(candidate_Y)
	if return_solution:
		return fixation_XY, warping_path
	return fixation_XY

def mode(values):
	values = list(values)
	return max(set(values), key=values.count)

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
