'''
Code for rendering PDFs showing the orignal fixation sequences and the
manual and algorithmic corrections.
'''

import eyekit
import tools

figure_layout = [[     'sample',  'gold'       ],
                 ['attach',  'chain', 'cluster'],
                 ['compare', 'merge', 'regress'],
                 ['segment', 'split', 'warp'   ]]

passages = tools.load_passages('../data/passages/')
datasets = {dataset : tools.load_data('../data/fixations/%s.json'%dataset) for dataset in [c for r in figure_layout for c in r]}

for passage_id, participants in datasets['sample'].items():
	print(passage_id)
	for participant_id, sample_fixations in participants.items():
		print('-', participant_id)
		sample_fixation_sequence = eyekit.FixationSequence(sample_fixations)
		gold_fixation_sequence = eyekit.FixationSequence(datasets['gold'][passage_id][participant_id])
		diagrams = []
		for row in figure_layout:
			diagrams.append([])
			for algorithm in row:
				diagram = eyekit.Diagram(1920, 1080)
				diagram.render_passage(passages[passage_id], 28, color='gray')
				if algorithm == 'sample':
					age = 'adult' if int(participant_id) < 100 else 'child'
					diagram.render_fixations(sample_fixation_sequence, include_discards=True)
					diagram.set_label('Passage %s, participant %s (%s)' % (passage_id, participant_id, age))
				elif algorithm == 'gold':
					diagram.render_fixations(gold_fixation_sequence, include_discards=True)
					diagram.set_label('Manual correction')
				else:
					data = datasets[algorithm]
					fixation_sequence = eyekit.FixationSequence(data[passage_id][participant_id])
					diagram.render_fixation_comparison(gold_fixation_sequence, fixation_sequence)
					diagram.set_label('<tspan style="font-family:Menlo">%s</tspan>' % algorithm)
				diagram.crop_to_passage(40)
				diagrams[-1].append(diagram)
		filepath = '../visuals/corrections/%s_%s.pdf' % (passage_id, participant_id)
		eyekit.diagram.combine_diagrams(diagrams, filepath, diagram_width=174, v_padding=3, h_padding=3, e_padding=10, auto_letter=False)
