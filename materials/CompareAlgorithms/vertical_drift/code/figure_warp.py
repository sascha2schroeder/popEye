import eyekit
import algorithms
import tools
import defaults

data = tools.load_data('../data/fixations/sample.json')
passages = tools.load_passages('../data/passages/')

fixation_sequence = eyekit.FixationSequence([f[:2] for f in data['1B']['8']])
corrected_fixation_sequence, solution = algorithms.correct_drift('warp', fixation_sequence.XYarray(), passages['1B'], return_solution=True)
corrected_fixation_sequence = eyekit.FixationSequence(corrected_fixation_sequence)
word_XY = passages['1B'].word_centers()

diagram = eyekit.Diagram(1920, 1080)
diagram.render_passage(passages['1B'], 28, color='gray')
diagram.render_fixations(eyekit.FixationSequence(word_XY), include_discards=True, color=defaults.illustration_colors[1])
diagram.render_fixations(fixation_sequence, include_discards=True, color=defaults.illustration_colors[2])
for fixation, mapped_words in zip(fixation_sequence, solution):
	for word_i in mapped_words:
		word_x, word_y = word_XY[word_i]
		diagram.draw_arbitrary_line(fixation.xy, (word_x, word_y), 'black', dashed=True)
diagram.crop_to_passage(40)

figure_layout = [[diagram]]
eyekit.diagram.combine_diagrams(figure_layout, '../visuals/illustration_warp.pdf',
	diagram_width=83, v_padding=3, h_padding=3, e_padding=1, auto_letter=False)
eyekit.diagram.combine_diagrams(figure_layout, '../manuscript/figs/illustration_warp.eps',
	diagram_width=83, v_padding=3, h_padding=3, e_padding=1, auto_letter=False)
