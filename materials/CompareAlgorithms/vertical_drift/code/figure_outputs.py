import eyekit
import tools

figure_layout = [['attach',  'chain', 'cluster'],
                 ['compare', 'merge', 'regress'],
                 ['segment', 'split', 'warp'   ]]

passages = tools.load_passages('../data/passages/')
gold_data = tools.load_data('../data/fixations/gold.json')

# Adult example

gold_fixation_sequence = eyekit.FixationSequence(gold_data['1B']['8'])
diagrams = []
for i, row in enumerate(figure_layout):
	diagrams.append([])
	for j, algorithm in enumerate(row):
		data = tools.load_data('../data/fixations/%s.json'%algorithm)
		fixation_sequence = eyekit.FixationSequence(data['1B']['8'])
		diagram = eyekit.Diagram(1920, 1080)
		diagram.render_passage(passages['1B'], 28, color='gray')
		diagram.render_fixation_comparison(gold_fixation_sequence, fixation_sequence)
		diagram.crop_to_passage(40)
		diagram.set_label('<tspan style="font-family:Menlo">%s</tspan>' % algorithm)
		diagrams[-1].append(diagram)
eyekit.diagram.combine_diagrams(diagrams, '../visuals/outputs_adult.pdf',
	diagram_width=174, v_padding=3, h_padding=3, e_padding=1)
eyekit.diagram.combine_diagrams(diagrams, '../manuscript/figs/outputs_adult.eps',
	diagram_width=174, v_padding=3, h_padding=3, e_padding=1)

# Child example

gold_fixation_sequence = eyekit.FixationSequence(gold_data['4A']['204'])
diagrams = []
for i, row in enumerate(figure_layout):
	diagrams.append([])
	for j, algorithm in enumerate(row):
		data = tools.load_data('../data/fixations/%s.json'%algorithm)
		fixation_sequence = eyekit.FixationSequence(data['4A']['204'])
		diagram = eyekit.Diagram(1920, 1080)
		diagram.render_passage(passages['4A'], 28, color='gray')
		diagram.render_fixation_comparison(gold_fixation_sequence, fixation_sequence)
		diagram.crop_to_passage(40)
		diagram.set_label('<tspan style="font-family:Menlo">%s</tspan>' % algorithm)
		diagrams[-1].append(diagram)
eyekit.diagram.combine_diagrams(diagrams, '../visuals/outputs_child.pdf',
	diagram_width=174, v_padding=3, h_padding=3, e_padding=1)
eyekit.diagram.combine_diagrams(diagrams, '../manuscript/figs/outputs_child.eps',
	diagram_width=174, v_padding=3, h_padding=3, e_padding=1)
