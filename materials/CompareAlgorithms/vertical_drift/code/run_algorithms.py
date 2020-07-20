'''
Code for running the algorithms over the sample data
'''

import numpy as np
from collections import defaultdict
import json
import eyekit
import algorithms
import tools
import defaults

def run_algorithm(sample_data, passages, output_dir, method):
	print(method.upper())
	output_data = defaultdict(dict)
	for passage_id, participant_data in sample_data.items():
		print(passage_id)
		for participant_id, fixations in participant_data.items():
			print('-', participant_id)
			fixation_sequence = eyekit.FixationSequence(fixations)
			fixation_XY = fixation_sequence.XYarray(include_discards=True)
			correction = algorithms.correct_drift(method, fixation_XY, passages[passage_id])
			for fixation, (x, y) in zip(fixation_sequence, correction):
				fixation.y = y

			output_data[passage_id][participant_id] = fixation_sequence.tolist(include_discards=True)
	with open(output_dir + '%s.json' % method, 'w') as file:
		json.dump(output_data, file)


if __name__ == '__main__':

	sample_file = '../data/fixations/sample.json'
	with open(sample_file) as file:
		sample_data = json.load(file)

	passages = tools.load_passages('../data/passages/')

	for method in defaults.algorithms:
		run_algorithm(sample_data, passages, '../data/fixations/', method)
