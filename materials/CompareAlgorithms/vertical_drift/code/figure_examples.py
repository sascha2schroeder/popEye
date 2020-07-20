import eyekit
import tools

data = tools.load_data('../data/fixations/sample.json')
passages = tools.load_passages('../data/passages/')

adult_fixation_sequence = eyekit.FixationSequence(data['1B']['8'])
child_fixation_sequence = eyekit.FixationSequence(data['4A']['204'])

adult = eyekit.Diagram(1920, 1080)
adult.render_passage(passages['1B'], 28, color='gray')
adult.render_fixations(adult_fixation_sequence, include_discards=True)
adult.crop_to_passage(40)
adult.set_label('Reading trial by an adult')

child = eyekit.Diagram(1920, 1080)
child.render_passage(passages['4A'], 28, color='gray')
child.render_fixations(child_fixation_sequence, include_discards=True)
child.crop_to_passage(40)
child.set_label('Reading trial by a child')

figure_layout = [[adult, child]]
eyekit.diagram.combine_diagrams(figure_layout, '../visuals/illustration_examples.pdf',
	diagram_width=174, v_padding=3, h_padding=3, e_padding=1)
eyekit.diagram.combine_diagrams(figure_layout, '../manuscript/figs/illustration_examples.eps',
	diagram_width=174, v_padding=3, h_padding=3, e_padding=1)
