'''
Creates visualizations for the sample trials and empty correction
files into which the two correctors can record their fixation-by-
fixation corrections.
'''

import eyekit
import tools

def create_empty_correction_file(corrector_id, fixation_sequence, participant_id, passage_id):
	participant_id = participant_id.zfill(3)
	correction_file = '../data/manual_corrections/%s/%s_%s' % (corrector_id, participant_id, passage_id)
	with open(correction_file, 'w') as file:
		for fixation in fixation_sequence:
			file.write('%s\t%s\t%s\t\n' % (str(round(fixation.duration)).zfill(4), str(round(fixation.x)).zfill(4), str(round(fixation.y)).zfill(4)))

def create_visualization(fixation_sequence, participant_id, passage_id):
	fig = eyekit.Diagram(1920, 1080)
	fig.render_passage(passages[passage_id], 28)
	fig.render_fixations(fixation_sequence, number_fixations=True)
	fig.save('../visuals/sample/%s_%s.pdf' % (participant_id, passage_id))

if __name__ == '__main__':

	correctors = ['JC', 'VP']

	passages = tools.load_passages('../data/passages/')
	sample_participants = tools.load_data('../data/fixations/sample.json')

	for passage_id, participants in sample_participants.items():
		for participant_id, fixations in participants.items():
			fixation_sequence = eyekit.FixationSequence(fixations)
			create_visualization(fixation_sequence, participant_id, passage_id)
			for corrector_id in correctors:
				create_empty_correction_file(corrector_id, fixation_sequence, participant_id, passage_id)
