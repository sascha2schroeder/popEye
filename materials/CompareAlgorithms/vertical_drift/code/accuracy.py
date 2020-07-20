'''
Code for calculating and visualizing how well the algorithms perform
against the gold standard manual correction.
'''

import matplotlib.pyplot as plt
import numpy as np
import json
import tools
import defaults

plt.rcParams['svg.fonttype'] = 'none' # don't convert fonts to curves in SVGs
plt.rcParams.update({'font.size': 7})

SPECIAL_ADULT = '8'
SPECIAL_KID = '204'

def percentage_match(line_assignments1, line_assignments2):
	matches = line_assignments1 == line_assignments2
	return matches.sum() / len(matches) * 100

def line_assignments(fixations):
	line_assignments = np.zeros(len(fixations), dtype=int)
	for i, fixation in enumerate(fixations):
		if fixation[3] == False:
			line_assignments[i] = defaults.y_to_line_mapping[fixation[1]]
	return line_assignments

def compare_outputs(method1, method2):
	with open('../data/fixations/%s.json'%method1) as file:
		data1 = json.load(file)
	with open('../data/fixations/%s.json'%method2) as file:
		data2 = json.load(file)
	results = {'adults':[], 'kids':[], 'adults_IDs':[], 'kids_IDs':[]}
	for passage_id, participant_data in data1.items():
		for participant_id, fixations in participant_data.items():
			line_assignments1 = line_assignments(fixations)
			line_assignments2 = line_assignments(data2[passage_id][participant_id])
			percentage = percentage_match(line_assignments1, line_assignments2)
			if int(participant_id) > 100:
				results['kids'].append(percentage)
				results['kids_IDs'].append(participant_id)
			else:
				results['adults'].append(percentage)
				results['adults_IDs'].append(participant_id)
	return results

def calculate_improvement(results):
	improvement_results = {}
	attach_adults = np.array(results['attach']['adults'], dtype=float)
	attach_kids = np.array(results['attach']['kids'], dtype=float)
	for algorithm in defaults.true_algorithms:
		assert results[algorithm]['adults_IDs'] == results['attach']['adults_IDs']
		assert results[algorithm]['kids_IDs'] == results['attach']['kids_IDs']
		alg_adults = np.array(results[algorithm]['adults'], dtype=float)
		alg_kids = np.array(results[algorithm]['kids'], dtype=float)
		improvement_adults = list(alg_adults - attach_adults)
		improvement_kids = list(alg_kids - attach_kids)
		improvement_results[algorithm] = {'adults': improvement_adults, 'kids':improvement_kids, 'adults_IDs':results[algorithm]['adults_IDs'], 'kids_IDs':results[algorithm]['kids_IDs']}
	return improvement_results

def plot_median_lines(axis, data, x_position, y_unit):
	adult_median = np.median(data['adults'])
	kid_median = np.median(data['kids'])
	offsets = ('bottom', 'top') if adult_median > kid_median else ('top', 'bottom')
	axis.plot([x_position-0.2, x_position+0.2], [adult_median, adult_median], color='black', linewidth=2)
	axis.text(x_position+0.25, adult_median, str(round(adult_median, 1)) + y_unit, ha='left', va=offsets[0], color='black', fontsize=7)
	axis.plot([x_position-0.2, x_position-0.05], [kid_median, kid_median], color='black', linewidth=2)
	axis.plot([x_position+0.05, x_position+0.2], [kid_median, kid_median], color='black', linewidth=2)
	axis.text(x_position+0.25, kid_median, str(round(kid_median, 1)) + y_unit, ha='left', va=offsets[1], color='black', fontsize=7)

def scatter_with_specials(axis, data, x_position, color, last_special_adult_result, last_special_kid_result):
	special_adult = data['adults_IDs'].index(SPECIAL_ADULT)
	special_kid = data['kids_IDs'].index(SPECIAL_KID)
	special_adult_result = data['adults'][special_adult]
	special_kid_result = data['kids'][special_kid]
	remaining_adult_results = data['adults'][:special_adult] + data['adults'][special_adult+1:]
	remaining_kid_results = data['kids'][:special_kid] + data['kids'][special_kid+1:]
	axis.scatter(np.random.normal(x_position, 0.07, len(remaining_adult_results)), remaining_adult_results, edgecolors=color, facecolors='none', s=8, linewidths=0.5)
	axis.scatter(np.random.normal(x_position, 0.07, len(remaining_kid_results)),   remaining_kid_results,   edgecolors=color, facecolors='none', s=8, linewidths=0.5, marker='^')
	axis.scatter(x_position, special_adult_result, color=color, s=8, linewidths=0.5)
	axis.scatter(x_position, special_kid_result, color=color, s=8, linewidths=0.5, marker='^')
	if last_special_adult_result:
		axis.plot([x_position-1, x_position], [last_special_adult_result, special_adult_result], color='#BBBBBB', linestyle='--', linewidth=0.5, zorder=0)
	if last_special_kid_result:
		axis.plot([x_position-1, x_position], [last_special_kid_result, special_kid_result], color='#BBBBBB', linestyle='--', linewidth=0.5, zorder=0)
	return special_adult_result, special_kid_result

def plot_legend(axis, legend_x, legend_y):
	axis.scatter([legend_x], [legend_y], marker='o', edgecolors='black', facecolors='none', s=8, linewidths=0.5, transform=axis.transAxes)
	axis.plot([legend_x+0.01, legend_x+0.04], [legend_y, legend_y], color='black', linewidth=1.5, transform=axis.transAxes)
	axis.text(legend_x+0.05, legend_y, 'Adults', ha='left', va='center', fontsize=7, transform=axis.transAxes)
	legend_y -= 0.05
	axis.scatter([legend_x], [legend_y], marker='^', edgecolors='black', facecolors='none', s=8, linewidths=0.5, transform=axis.transAxes)
	axis.plot([legend_x+0.01, legend_x+0.02125], [legend_y, legend_y], color='black', linewidth=1.5, transform=axis.transAxes)
	axis.plot([legend_x+0.02875, legend_x+0.04], [legend_y, legend_y], color='black', linewidth=1.5, transform=axis.transAxes)
	axis.text(legend_x+0.05, legend_y, 'Children', ha='left', va='center', fontsize=7, transform=axis.transAxes)

def plot_results(results, filepath, y_label, y_limits, y_unit):
	fig, axis = plt.subplots(1, 1, figsize=(6.8, 2.5))
	if y_limits[0] < 0:
		axis.plot([-1, len(results)], [0, 0], color='black', linewidth=0.5)
	special_adult, special_kid = None, None
	x_labels = []
	for x_position, (algorithm, data) in enumerate(results.items()):
		color = defaults.colors[algorithm]
		special_adult, special_kid = scatter_with_specials(axis, data, x_position, color, special_adult, special_kid)
		plot_median_lines(axis, data, x_position, y_unit)
		x_labels.append(algorithm)
	plot_legend(axis, 0.87, 0.1)
	offset = (y_limits[1] - y_limits[0]) / 20
	axis.set_ylim(y_limits[0]-offset, y_limits[1]+offset)
	axis.set_xlim(-0.5, len(x_labels)-0.3)
	axis.set_xticks(list(range(len(x_labels))))
	axis.tick_params(bottom=False)
	axis.set_xticklabels(x_labels)
	axis.set_ylabel(y_label)
	fig.tight_layout(pad=0.1, h_pad=0.5, w_pad=0.5)
	fig.savefig(filepath, format='svg')
	tools.format_svg_labels(filepath, defaults.algorithms)
	if not filepath.endswith('.svg'):
		tools.convert_svg(filepath, filepath)


if __name__ == '__main__':

	accuracy_results = {algorithm : compare_outputs('gold', algorithm) for algorithm in defaults.algorithms}
	improvement_results = calculate_improvement(accuracy_results)

	plot_results(accuracy_results, '../visuals/results_accuracy.pdf', 'Accuracy of algorithmic correction (%)', (0, 100), '%')
	plot_results(accuracy_results, '../manuscript/figs/results_accuracy.eps', 'Accuracy of algorithmic correction (%)', (0, 100), '%')

	plot_results(improvement_results, '../visuals/results_improvement.pdf', 'Percentage point improvement in accuracy', (-80, 80), 'pp')
	plot_results(improvement_results, '../manuscript/figs/results_improvement.eps', 'Percentage point improvement in accuracy', (-80, 80), 'pp')
