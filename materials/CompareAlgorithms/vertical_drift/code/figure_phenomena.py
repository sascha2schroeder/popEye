import eyekit
import simulation
import defaults


def run_and_visualize(passage, label, **params):
	reading_scenario = simulation.ReadingScenario(**params)
	_, fixation_sequence, _ = reading_scenario.simulate(passage)
	fixation_sequence = eyekit.FixationSequence(fixation_sequence)
	diagram = eyekit.Diagram(1920, 1080)
	diagram.render_passage(passage, 28, color='gray')
	diagram.render_fixations(fixation_sequence, color='black')
	diagram.crop_to_passage(20)
	diagram.set_label(label)
	return diagram


passage = eyekit.Passage(defaults.lorem_ipsum_text,
	                     first_character_position=(368, 155),
	                     character_spacing=16,
	                     line_spacing=64,
	                     pad_lines_with_spaces=True)

noise_diagram = run_and_visualize(passage, 'Noise', noise=20.0)
slope_diagram = run_and_visualize(passage, 'Slope (downward)', noise=2.0, slope=0.1)
shift_diagram = run_and_visualize(passage, 'Shift (downward)', noise=2.0, shift=0.3)
within_diagram = run_and_visualize(passage, 'Within-line regression', noise=2.0, regression_within=0.5)
between_diagram = run_and_visualize(passage, 'Between-line regression', noise=2.0, regression_between=0.5)

figure_layout = [[noise_diagram],
                 [slope_diagram],
                 [shift_diagram],
                 [within_diagram],
                 [between_diagram]]

eyekit.diagram.combine_diagrams(figure_layout, '../visuals/illustration_phenomena.pdf',
	diagram_width=83, v_padding=3, h_padding=3, e_padding=1)
eyekit.diagram.combine_diagrams(figure_layout, '../manuscript/figs/illustration_phenomena.eps',
	diagram_width=83, v_padding=3, h_padding=3, e_padding=1)
