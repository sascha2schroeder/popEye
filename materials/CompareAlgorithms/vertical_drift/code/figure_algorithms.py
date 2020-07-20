import numpy as np
import json
import eyekit
import algorithms
import defaults

def visualize_attach(passage, fixation_sequence):
	corrected_sequence, solution = algorithms.correct_drift('attach', fixation_sequence.XYarray(), passage, return_solution=True)
	corrected_sequence = eyekit.FixationSequence(corrected_sequence)
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	for fixation, fixation2 in zip(fixation_sequence, corrected_sequence):
		color = defaults.illustration_colors[defaults.y_to_line_mapping[fixation2.y]-1]
		diagram.draw_arbitrary_circle(fixation.xy, 5.641895835477563, color)
		diagram.draw_arbitrary_line(fixation.xy, fixation2.xy, color)
	diagram.crop_to_passage(20)
	diagram.set_label('<tspan style="font-family:Menlo">attach</tspan>')
	return diagram

def visualize_chain(passage, fixation_sequence):
	corrected_sequence, solution = algorithms.correct_drift('chain', fixation_sequence.XYarray(), passage, return_solution=True, x_thresh=100)
	corrected_sequence = eyekit.FixationSequence(corrected_sequence)
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	prev = None
	for i, (fixation, fixation2) in enumerate(zip(fixation_sequence, corrected_sequence)):
		color = defaults.illustration_colors[defaults.y_to_line_mapping[fixation2.y]-1]
		diagram.draw_arbitrary_circle(fixation.xy, 5.641895835477563, color)
		if prev and i not in solution:
			diagram.draw_arbitrary_line(prev.xy, fixation.xy, color)
		prev = fixation
	diagram.crop_to_passage(20)
	diagram.set_label('<tspan style="font-family:Menlo">chain</tspan>')
	return diagram

def visualize_cluster(passage, fixation_sequence):
	corrected_sequence, solution = algorithms.correct_drift('cluster', fixation_sequence.XYarray(), passage, return_solution=True)
	corrected_sequence = eyekit.FixationSequence(corrected_sequence)
	clusters, ordered_cluster_indices = solution
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	for fixation, cluster_i in zip(fixation_sequence, clusters):
		line_i = ordered_cluster_indices.index(cluster_i)
		diagram.draw_arbitrary_circle(fixation.xy, 5.641895835477563, color=defaults.illustration_colors[line_i])
	for line_i, cluster_i in enumerate(ordered_cluster_indices):
		fixations_in_cluster = np.where(clusters == cluster_i)[0]
		y_values = [fixation_sequence[int(f)].y for f in fixations_in_cluster]
		mn, mx = min(y_values), max(y_values)
		diagram.draw_arbitrary_rectangle(344, mn, 656, mx-mn, color=defaults.illustration_colors[line_i], dashed=True)
	diagram.crop_to_passage(20)
	diagram.set_label('<tspan style="font-family:Menlo">cluster</tspan>')
	return diagram

def visualize_compare(passage, fixation_sequence):
	_, solution = algorithms.correct_drift('compare', fixation_sequence.XYarray(), passage, return_solution=True, x_thresh=300)
	word_XY = passage.word_centers()
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	gaze_line = solution[0][0]
	text_lines = solution[0][1]
	warp_paths = solution[0][2]
	line_costs = solution[0][3]
	for color_i, (text_line, warp_path) in enumerate(zip(text_lines, warp_paths)):
		for fixation, mapped_words in zip(gaze_line, warp_path):
			for word_i in mapped_words:
				word_x, word_y = text_line[word_i]
				diagram.draw_arbitrary_circle((word_x, word_y), 5.641895835477563, color='gray')
				diagram.draw_arbitrary_line(tuple(fixation), (word_x, fixation[1]), color=defaults.illustration_colors[color_i])
		for fixation in gaze_line:
			diagram.draw_arbitrary_circle(tuple(fixation), 5.641895835477563, color='black')
		diagram.draw_arbitrary_text(1000, text_line[0][1], int(line_costs[color_i]), color=defaults.illustration_colors[color_i], align='end', css_style={'font-size':'30', 'font-family':'Helvetica Neue', 'font-weight':'bold'})
		gaze_line[:, 1] += 64
	diagram.crop_to_passage(20)
	diagram.set_label('<tspan style="font-family:Menlo">compare</tspan> (gaze line 1)')
	return diagram

def visualize_merge(passage, fixation_sequence):
	corrected_sequence, solution = algorithms.correct_drift('merge', fixation_sequence.XYarray(), passage, return_solution=True)
	corrected_sequence = eyekit.FixationSequence(corrected_sequence)
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	for line_i, sequence in solution.items():
		color = defaults.illustration_colors[line_i]
		diagram.draw_arbitrary_circle(fixation_sequence[sequence[0]].xy, 5.641895835477563, color)
		for i in range(1, len(sequence)):
			diagram.draw_arbitrary_line(fixation_sequence[sequence[i-1]].xy, fixation_sequence[sequence[i]].xy, color)
			diagram.draw_arbitrary_circle(fixation_sequence[sequence[i]].xy, 5.641895835477563, color)
		fixation_XY = fixation_sequence.XYarray()[sequence]
		k, intercept = np.polyfit(fixation_XY[:, 0], fixation_XY[:, 1], 1)
		start = (344, intercept+(344*k))
		end = (1000, intercept+(1000*k))
		diagram.draw_arbitrary_line(start, end, color=color, dashed=True)
	diagram.crop_to_passage(20)
	diagram.set_label('<tspan style="font-family:Menlo">merge</tspan> (final merged sequences)')
	return diagram

def visualize_regress(passage, fixation_sequence):
	corrected_sequence, solution = algorithms.correct_drift('regress', fixation_sequence.XYarray(), passage, return_solution=True)
	corrected_sequence = eyekit.FixationSequence(corrected_sequence)
	start_points = np.array([(0, y) for y in passage.line_positions])
	(k, o, s), line_numbers = solution
	start_points[:, 1] = start_points[:, 1] + o
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	for start_point, color in zip(start_points, defaults.illustration_colors):
		start = (344, start_point[1]+(344*k))
		end = (1000, start_point[1]+(1000*k))
		diagram.draw_arbitrary_line(start, end, color=color, dashed=True)
	for fixation, line_i in zip(fixation_sequence, line_numbers):
		diagram.draw_arbitrary_circle(fixation.xy, 5.641895835477563, color=defaults.illustration_colors[line_i])
		predicted_y = (fixation.x * k) + passage.line_positions[line_i] + o
		diagram.draw_arbitrary_line(fixation.xy, (fixation.x, predicted_y), color=defaults.illustration_colors[line_i])
	diagram.crop_to_passage(20)
	diagram.set_label('<tspan style="font-family:Menlo">regress</tspan>')
	return diagram

def visualize_segment(passage, fixation_sequence):
	corrected_sequence, solution = algorithms.correct_drift('segment', fixation_sequence.XYarray(), passage, return_solution=True)
	corrected_sequence = eyekit.FixationSequence(corrected_sequence)
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	for i, line_change_i in enumerate(solution):
		fxn = int(line_change_i)
		diagram.draw_arbitrary_line(fixation_sequence[fxn].xy, fixation_sequence[fxn+1].xy, 'black')
	for fixation, fixation2 in zip(fixation_sequence, corrected_sequence):
		color = defaults.illustration_colors[defaults.y_to_line_mapping[fixation2.y]-1]
		diagram.draw_arbitrary_circle(fixation.xy, 5.641895835477563, color)
	diagram.crop_to_passage(20)
	diagram.set_label('<tspan style="font-family:Menlo">segment</tspan>')
	return diagram

def visualize_split(passage, fixation_sequence):
	corrected_sequence, solution = algorithms.correct_drift('split', fixation_sequence.XYarray(), passage, return_solution=True)
	corrected_sequence = eyekit.FixationSequence(corrected_sequence)
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	for i, line_change_i in enumerate(solution[:-1]):
		fxn = int(line_change_i) - 1
		diagram.draw_arbitrary_line(fixation_sequence[fxn].xy, fixation_sequence[fxn+1].xy, 'black')
	for fixation, fixation2 in zip(fixation_sequence, corrected_sequence):
		color = defaults.illustration_colors[defaults.y_to_line_mapping[fixation2.y]-1]
		diagram.draw_arbitrary_circle(fixation.xy, 5.641895835477563, color)
	diagram.crop_to_passage(20)
	diagram.set_label('<tspan style="font-family:Menlo">split</tspan>')
	return diagram

def visualize_warp(passage, fixation_sequence):
	_, solution = algorithms.correct_drift('warp', fixation_sequence.XYarray(), passage, return_solution=True)
	word_XY = passage.word_centers()
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	for word in passage.word_centers():
		diagram.draw_arbitrary_circle(word, 5.641895835477563, color='gray')
	for fixation, mapped_words in zip(fixation_sequence, solution):
		for word_i in mapped_words:
			word_x, word_y = word_XY[word_i]
			color = defaults.illustration_colors[defaults.y_to_line_mapping[word_y]-1]
			diagram.draw_arbitrary_circle(fixation.xy, 5.641895835477563, color)
			diagram.draw_arbitrary_line(fixation.xy, (word_x, word_y), color)
	diagram.crop_to_passage(20)
	diagram.set_label('<tspan style="font-family:Menlo">warp</tspan>')
	return diagram


passage = eyekit.Passage(defaults.lorem_ipsum_text,
	                     first_character_position=(368, 155),
	                     character_spacing=16,
	                     line_spacing=64,
	                     pad_lines_with_spaces=True)

fixation_sequence = eyekit.FixationSequence(defaults.lorem_ipsum_XY)

attach = visualize_attach(passage, fixation_sequence)
chain = visualize_chain(passage, fixation_sequence)
cluster = visualize_cluster(passage, fixation_sequence)
compare = visualize_compare(passage, fixation_sequence)
merge = visualize_merge(passage, fixation_sequence)
regress = visualize_regress(passage, fixation_sequence)
segment = visualize_segment(passage, fixation_sequence)
split = visualize_split(passage, fixation_sequence)
warp = visualize_warp(passage, fixation_sequence)

figure_layout = [[attach,  chain, cluster],
                 [compare, merge, regress],
                 [segment, split, warp   ]]

eyekit.diagram.combine_diagrams(figure_layout, '../visuals/illustration_algorithms.pdf',
	diagram_width=174, v_padding=3, h_padding=3, e_padding=1)
eyekit.diagram.combine_diagrams(figure_layout, '../manuscript/figs/illustration_algorithms.eps',
	diagram_width=174, v_padding=3, h_padding=3, e_padding=1)
