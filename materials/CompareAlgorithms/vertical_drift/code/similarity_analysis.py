'''
Code for comparing the algorithmic outputs in an MDS and hierarchical
clustering analysis.
'''

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.transforms as transforms
from sklearn.manifold import MDS
from scipy.spatial import distance
from scipy.cluster import hierarchy
import json
import tools
import defaults
from algorithms import dynamic_time_warping

plt.rcParams['svg.fonttype'] = 'none' # don't convert fonts to curves in SVGs
plt.rcParams.update({'font.size': 7})


class Dendrogram:

	def __init__(self, linkage_matrix, object_names, object_colors):
		self.linkage_matrix = linkage_matrix
		self.tree = hierarchy.to_tree(linkage_matrix)
		self.object_names = object_names
		self.object_colors = object_colors
		self.node_points = {}
		self._vertical_positions(self.tree)
		self._horizontal_positions()

	def _vertical_positions(self, node, level=0):
		self.node_points[node.get_id()] = [None, -level]
		if not node.is_leaf():
			self._vertical_positions(node.get_left(), level+1)
			self._vertical_positions(node.get_right(), level+1)

	def _horizontal_positions(self):
		_, axis = plt.subplots(1, 1)
		dg = hierarchy.dendrogram(self.linkage_matrix, ax=axis)
		leaf_X = axis.get_xticks()
		positions = dict(zip(dg['leaves'], leaf_X))
		for i, merger in enumerate(self.linkage_matrix, len(dg['leaves'])):
			positions[i] = np.mean([positions[int(merger[0])], positions[int(merger[1])]])
		for node_id in positions.keys():
			self.node_points[node_id][0] = positions[node_id]

	def _recursive_adjust(self, node, target, adjustment, found=False):
		node_id = node.get_id()
		L, R = node.get_left(), node.get_right()
		if found:
			self.node_points[node_id][0] += adjustment
			if L.is_leaf():
				self.node_points[L.get_id()][0] += adjustment
			if R.is_leaf():
				self.node_points[R.get_id()][0] += adjustment
		if node_id == target:
			found = True
		if not L.is_leaf():
			self._recursive_adjust(L, target, adjustment, found)
		if not R.is_leaf():
			self._recursive_adjust(R, target, adjustment, found)

	def _recursive_plot(self, node, axis):
		node_id = node.get_id()
		if node.is_leaf():
			x, y = self.node_points[node_id]
			object_name = self.object_names[node_id]
			axis.scatter([x], [y], c=self.object_colors[object_name])
			axis.annotate(object_name, (x, y-0.2), va='top', ha='center', fontsize=7)
			return # Leaf node, break recursion
		L, R = node.get_left(), node.get_right()
		left_node_id, right_node_id = L.get_id(), R.get_id()
		X = [self.node_points[left_node_id][0], self.node_points[node_id][0], self.node_points[right_node_id][0]]
		Y = [self.node_points[left_node_id][1], self.node_points[node_id][1], self.node_points[right_node_id][1]]
		axis.plot(X, Y, c='black', zorder=0)
		self._recursive_plot(L, axis)
		self._recursive_plot(R, axis)

	def adjust(self, node_id, adjustment):
		'''
		Move the x-position of a given node by some amount. This can be used
		to tidy up a diagram.
		'''
		if node_id >= len(self.object_names): # branch node
			self._recursive_adjust(self.tree, node_id, adjustment)
		else: # leaf node
			self.node_points[node_id][0] += adjustment

	def plot(self, axis):
		'''
		Plot the dendrogram on a given axis.
		'''
		self._recursive_plot(self.tree, axis)
		branch_nodes = zip(*[xy for node_id, xy in self.node_points.items() if node_id >= len(self.object_names)])
		axis.scatter(*branch_nodes, c='black')
		min_x, max_x = axis.get_xlim()
		axis.set_xlim(min_x-2, max_x+2)
		min_y, max_y = axis.get_ylim()
		axis.set_ylim(min_y-0.25, 0.25)
		axis.set_xticks([])
		axis.set_yticks([])


def algorithmic_output_distance(method1, method2):
	with open('../data/fixations/%s.json'%method1) as file:
		data1 = json.load(file)
	with open('../data/fixations/%s.json'%method2) as file:
		data2 = json.load(file)
	results = []
	for passage_id, participant_data in data1.items():
		for participant_id, fixations in participant_data.items():
			fixation_XY1 = np.array([(f[0], f[1]) for f in fixations if not f[3]], dtype=int)
			fixation_XY2 = np.array([(f[0], f[1]) for f in data2[passage_id][participant_id] if not f[3]], dtype=int)
			cost, _ = dynamic_time_warping(fixation_XY1, fixation_XY2)
			results.append(cost)
	return np.median(results)

def make_algorithmic_distance_matrix(methods, filepath):
	distances = []
	for m1 in range(len(methods)):
		print(methods[m1])
		for m2 in range(m1+1, len(methods)):
			print('-', methods[m2])
			distance = algorithmic_output_distance(methods[m1], methods[m2])
			distances.append(distance)
	matrix = distance.squareform(distances, 'tomatrix')
	tools.pickle((methods, matrix), filepath)

def min_max_normalize(positions):
	for i in range(positions.shape[1]):
		positions[:, i] = (positions[:, i] - positions[:, i].min()) / (positions[:, i].max() - positions[:, i].min())
	return positions

def subset_distance_matrix(algorithm_distances, subset_methods):
	original_methods, matrix = algorithm_distances
	new_matrix = np.zeros((len(subset_methods), len(subset_methods)), dtype=float)
	for i, method1 in enumerate(subset_methods):
		original_i = original_methods.index(method1)
		for j, method2 in enumerate(subset_methods):
			original_j = original_methods.index(method2)
			new_matrix[i, j] = matrix[original_i, original_j]
	return new_matrix

def hierarchical_clustering_analysis(algorithm_distances, methods):
	matrix = subset_distance_matrix(algorithm_distances, methods)
	condensed_matrix = distance.squareform(matrix, 'tovector')
	return methods, hierarchy.linkage(condensed_matrix, method='centroid')

def multidimensional_scaling_analysis(algorithm_distances, methods, random_seed=117):
	matrix = subset_distance_matrix(algorithm_distances, methods)
	mds = MDS(dissimilarity='precomputed', n_components=2, n_init=25, max_iter=2000, random_state=random_seed)
	positions = mds.fit_transform(matrix)
	return methods, min_max_normalize(positions)

def plot_analyses(ahc_solution, mds_solution, filepath):
	fig, axes = plt.subplots(1, 2, figsize=(6.8, 2.5))

	# Plot AHC clustering
	ahc_methods, linkage_matrix = ahc_solution
	method_names = {i:method for i, method in enumerate(ahc_methods)}
	dendrogram = Dendrogram(linkage_matrix, method_names, defaults.colors)
	dendrogram.adjust(0, -2) # Manual adjustments for prettiness
	dendrogram.adjust(1, 2)
	dendrogram.adjust(2, -2)
	dendrogram.adjust(4, 2)
	dendrogram.adjust(5, -2)
	dendrogram.adjust(6, 8)
	dendrogram.adjust(7, 2)
	dendrogram.adjust(13, -8)
	dendrogram.plot(axes[0])
	for node, label in [(8, 'Sequential'), (13, 'Positional'), (10, 'Relative'), (11, 'Absolute')]:
		x, y = dendrogram.node_points[node]
		axes[0].text(x+3, y, label, ha='left', va='center')
	inches_from_origin = (fig.dpi_scale_trans + transforms.ScaledTranslation(0, 1, axes[0].transAxes))
	axes[0].text(0.1, -0.1, '(A)', fontsize=8, fontweight='bold', ha='left', va='top', transform=inches_from_origin)
	
	# Plot MDS solution
	mds_methods, positions = mds_solution
	mn, mx = positions[:, 0].min(), positions[:, 0].max()
	offset = (mx - mn) * 0.1
	furthest_method_to_right = mds_methods[np.argmax(positions[:, 0])]
	axes[1].scatter(positions[:, 0], positions[:, 1], color=[defaults.colors[m] for m in mds_methods])
	for label, position in zip(mds_methods, positions):
		if label == furthest_method_to_right:
			axes[1].text(position[0]-offset/3, position[1], label, va='center', ha='right')
		else:
			axes[1].text(position[0]+offset/3, position[1], label, va='center', ha='left')
	axes[1].set_xlim(mn-offset, mx+offset)
	mn, mx = positions[:, 1].min(), positions[:, 1].max()
	offset = (mx - mn) * 0.1
	axes[1].set_ylim(mn-offset, mx+offset)
	axes[1].set_xticks([])
	axes[1].set_yticks([])
	inches_from_origin = (fig.dpi_scale_trans + transforms.ScaledTranslation(0, 1, axes[1].transAxes))
	axes[1].text(0.1, -0.1, '(B)', fontsize=8, fontweight='bold', ha='left', va='top', transform=inches_from_origin)

	fig.tight_layout(pad=0.1, h_pad=0.5, w_pad=0.5)
	fig.savefig(filepath, format='svg')
	tools.format_svg_labels(filepath, monospace=defaults.algorithms, arbitrary_replacements={'gold':'Gold standard', 'JC':'Jon', 'VP':'Vale'})
	if not filepath.endswith('.svg'):
		tools.convert_svg(filepath, filepath)


if __name__ == '__main__':

	# Measure pairwise distances between methods and pickle the distance matrix
	# make_algorithmic_distance_matrix(defaults.algorithms+['gold'], '../data/algorithm_distances.pkl')

	# Load the distance matrix created in the above step
	algorithm_distances = tools.unpickle('../data/algorithm_distances.pkl')

	# Compute the hierarchical clustering solution
	ahc_solution = hierarchical_clustering_analysis(algorithm_distances, defaults.good_algorithms)

	# Compute the multidimensional scaling solution
	mds_solution = multidimensional_scaling_analysis(algorithm_distances, defaults.good_algorithms+['gold'], random_seed=11)

	# Plot the analyses
	plot_analyses(ahc_solution, mds_solution, '../visuals/results_similarity.pdf')
	plot_analyses(ahc_solution, mds_solution, '../manuscript/figs/results_similarity.eps')
