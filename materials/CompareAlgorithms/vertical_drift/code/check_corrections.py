'''
Code for validating the manual corrections and converting them into
JSON files for analysis. Validation includes:

- All correction files are present
- Lines in the correction file are present and correctly ordered
- Correction file is correctly formatted (e.g. tab separated numbers)
- Correct number of line assignments (e.g. passage with 10 lines
  should have assignments from 1 to 10 and maybe 0s)
'''

from collections import defaultdict
from os import listdir, path
import json
import eyekit
import tools
import defaults


def iter_correction_path(correction_path):
	for correction_file in listdir(correction_path):
		if correction_file.startswith('.'):
			continue
		participant_id, passage_id = correction_file.split('_')
		participant_id = str(int(participant_id))
		file_path = path.join(correction_path, correction_file)
		yield file_path, participant_id, passage_id

def check_correction(correction_path, participant_data, passages):
	for file_path, participant_id, passage_id in iter_correction_path(correction_path):
		fixation_sequence = eyekit.FixationSequence(participant_data[passage_id][participant_id])
		line_numbers = []
		with open(file_path) as file:
			for i, (line, fixation) in enumerate(zip(file, fixation_sequence), 1):
				try:
					d, x, y, l = line.split('\t')
					d, x, y, l = int(d), int(x), int(y), int(l)
					assert x == fixation.x
					assert y == fixation.y
					assert d == fixation.duration
				except:
					print('Error on line %i in %s' % (i, file_path))
					quit()
				line_numbers.append(l)
		line_numbers = sorted(list(set(line_numbers)))
		try:
			assert (len(line_numbers) == max(line_numbers) and 0 not in line_numbers) or (len(line_numbers) == max(line_numbers)+1)
		except AssertionError:
			print('Warning: %s only contains line numbers %s' % (file_path, str(line_numbers)))
		try:
			assert max(line_numbers) == passages[passage_id].n_rows
		except:
			print('Warning: %s doesn\'t match number of lines in passage' % file_path)

def make_corrected_file(correction_path, corrected_file_path, passages):
	data = defaultdict(dict)
	for file_path, participant_id, passage_id in iter_correction_path(correction_path):
		fixation_sequence = eyekit.FixationSequence(participant_data[passage_id][participant_id])
		with open(file_path) as file:
			for i, (line, fixation) in enumerate(zip(file, fixation_sequence), 1):
				l = int(line.split('\t')[3])
				if l == 0:
					fixation.discarded = True
				else:
					fixation.y = passages[passage_id].line_positions[l-1]
		data[passage_id][participant_id] = fixation_sequence.tolist(include_discards=True)
	passage_ids = list(data.keys())
	passage_ids.sort()
	new_json = {}
	for passage_id in passage_ids:
		participant_ids = list(data[passage_id].keys())
		participant_ids = list(map(int, participant_ids))
		participant_ids.sort()
		participant_ids = list(map(str, participant_ids))
		new_json[passage_id] = {participant_id : data[passage_id][participant_id] for participant_id in participant_ids}
	with open(corrected_file_path, 'w') as file:
		json.dump(new_json, file)

def compare_two_corrections(correction1_file_path, correction2_file_path):
	with open(correction1_file_path) as file:
		correction1 = json.load(file)
	with open(correction2_file_path) as file:
		correction2 = json.load(file)
	for passage_id, participant_data in correction1.items():
		for participant_id, fixations in participant_data.items():
			print('Participant %s, passage %s'%(participant_id, passage_id))
			for i, fixation1 in enumerate(fixations):
				fixation2 = correction2[passage_id][participant_id][i]
				if fixation1[3]:
					line_assignment1 = 0
				else:
					line_assignment1 = defaults.y_to_line_mapping[fixation1[1]]
				if fixation2[3]:
					line_assignment2 = 0
				else:
					line_assignment2 = defaults.y_to_line_mapping[fixation2[1]]
				if line_assignment1 != line_assignment2:
					if line_assignment1 == 0 or line_assignment2 == 0:
						print(' - Fixation %i: JC says %i, VP says %i (difference in discarding)'%((i+1), line_assignment1, line_assignment2))
					else:
						print(' - Fixation %i: JC says %i, VP says %i (difference in line assignment)'%((i+1), line_assignment1, line_assignment2))


if __name__ == '__main__':

	passages = tools.load_passages('../data/passages/')
	participant_data = tools.load_data('../data/fixations/sample_striped.json')

	# Step 1: Check validity of JC's correction and make JSON file
	correction_path = '../data/manual_corrections/JC/'
	corrected_file_path = '../data/fixations/JC.json'
	check_correction(correction_path, participant_data, passages)
	make_corrected_file(correction_path, corrected_file_path, passages)

	# Step 2: Check validity of VP's correction and make JSON file
	correction_path = '../data/manual_corrections/VP/'
	corrected_file_path = '../data/fixations/VP.json'
	check_correction(correction_path, participant_data, passages)
	make_corrected_file(correction_path, corrected_file_path, passages)

	# Step 3: Compare JC and VP's corrections and highlight any differences
	compare_two_corrections('../data/fixations/JC.json', '../data/fixations/VP.json')

	# Step 4: Create the gold standard JSON file from a merger of the two corrections
	correction_path = '../data/manual_corrections/gold/'
	corrected_file_path = '../data/fixations/gold.json'
	check_correction(correction_path, participant_data, passages)
	make_corrected_file(correction_path, corrected_file_path, passages)
