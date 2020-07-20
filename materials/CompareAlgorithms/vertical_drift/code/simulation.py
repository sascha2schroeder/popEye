'''
Code for performing the fixation sequence simulations.
'''

from sys import stdout
import numpy as np
import eyekit
import lorem
from tools import pickle
import algorithms
import defaults


class ReadingScenario:

	def __init__(self, noise=0, slope=0, shift=0, regression_within=0, regression_between=0, lines_per_passage=(8, 12), max_characters_per_line=80, character_spacing=16, line_spacing=64):
		# Distortion parameters
		self.noise = noise
		self.slope = slope
		self.shift = shift
		# Regression parameters
		self.regression_within = regression_within
		self.regression_between = regression_between
		# Passage parameters
		self.min_lines, self.max_lines = lines_per_passage
		self.max_characters_per_line = max_characters_per_line
		self.character_spacing = character_spacing
		self.line_spacing = line_spacing

	def _generate_passage(self):
		n_lines = np.random.randint(self.min_lines, self.max_lines+1)
		lines = ['']
		while len(lines) < n_lines:
			for word in lorem.sentence().split():
				if (len(lines[-1]) + len(word)) <= self.max_characters_per_line:
					lines[-1] += word + ' '
				else:
					lines.append(word + ' ')
			if len(lines) == n_lines and len(lines[-1].split()) == 1:
				# Final line only contains one word, so add in an extra word
				# because a one-word final line can be problematic for merge
				# since it cannot create sequences with one fixation.
				lines[-1] = 'lorem ' + lines[-1]
		return eyekit.Passage(lines, first_character_position=(0, 0), character_spacing=self.character_spacing, line_spacing=self.line_spacing)

	def _generate_line_sequence(self, passage, line_i, partial_reading=False, inherited_line_y_for_shift=None):
		x_margin, y_margin = passage.first_character_position
		max_line_width = passage.character_spacing * passage.n_cols
		if partial_reading:
			start_point = np.random.randint(0, max_line_width//2) + x_margin
			end_point = np.random.randint(max_line_width//2, max_line_width) + x_margin
		else:
			start_point = x_margin
			end_point = max_line_width + x_margin
		line_X = []
		for word_i, word in enumerate(passage.iter_words(line_n=line_i)):
			x_word_center = word[0].x + ((word[-1].x - word[0].x) / 2)
			if x_word_center < start_point or x_word_center > end_point:
				continue
			x_value = int(np.random.triangular(word[0].x, x_word_center, word[-1].x+1))
			line_X.append(x_value)
			if word_i > 0 and np.random.random() < self.regression_within:
				x_regression = int(np.random.triangular(x_margin, word[0].x, word[0].x))
				line_X.append(x_regression)
		line_X = np.array(line_X, dtype=int) - x_margin
		line_y = passage.line_positions[line_i] - y_margin
		line_Y = np.random.normal(line_y, self.noise, len(line_X))
		line_Y += line_X * self.slope
		if inherited_line_y_for_shift:
			line_Y += (inherited_line_y_for_shift - y_margin) * self.shift
		else:
			line_Y += line_y * self.shift
		line_Y = np.array(list(map(round, line_Y)), dtype=int)
		return line_X + x_margin, line_Y + y_margin, [line_y]*len(line_Y)

	def _generate_fixation_sequence(self, passage):
		X, Y, intended_Y = [], [], []
		for line_i, line_y in enumerate(passage.line_positions):
			line_X, line_Y, line_I = self._generate_line_sequence(passage, line_i)
			X.extend(line_X)
			Y.extend(line_Y)
			intended_Y.extend(line_I)
			if line_i > 0 and np.random.random() < self.regression_between:
				rand_prev_line = int(np.random.triangular(0, line_i, line_i))
				rand_insert_point = np.random.randint(1, len(line_X))
				regression = self._generate_line_sequence(passage, rand_prev_line, partial_reading=True, inherited_line_y_for_shift=line_y)
				for rx, ry, ri in zip(*regression):
					X.insert(-rand_insert_point, rx)
					Y.insert(-rand_insert_point, ry)
					intended_Y.insert(-rand_insert_point, ri)
		return np.column_stack([X, Y]), np.array(intended_Y, dtype=int)

	def simulate(self, passage=None):
		'''
		Generates a fixation sequence over a passage using the distortion
		and regression parameters of the reading scenario. If no passsage
		is provided, a random one is generated according to the passage
		parameters of the reading scenario. Returns the passage, fixation
		sequence, and "correct" lines numbers.
		'''
		if passage is None:
			passage = self._generate_passage()
		fixation_XY, intended_Y = self._generate_fixation_sequence(passage)
		return passage, fixation_XY, intended_Y


def simulate_factor(factor, n_gradations, n_sims):
	'''
	Performs some number of simulations for each gradation in the factor
	space. A reading scenario is created for each factor value, and then,
	for each simulation, a passage and fixation sequence are generated
	and corrected by each algorithm. Results are returned as a 3D numpy
	array.
	'''
	results = np.zeros((len(defaults.algorithms), n_gradations, n_sims), dtype=float)
	_, (factor_min, factor_max) = defaults.factors[factor]
	for gradation_i, factor_value in enumerate(np.linspace(factor_min, factor_max, n_gradations)):
		print('%s = %f' % (factor, factor_value))
		reading_scenario = ReadingScenario(**{factor:factor_value})
		for sim_i in range(n_sims):
			passage, fixation_XY, intended_Y = reading_scenario.simulate()
			for method_i, method in enumerate(defaults.algorithms):
				corrected_XY = algorithms.correct_drift(method, fixation_XY, passage)
				matches = intended_Y == corrected_XY[:, 1]
				results[method_i][gradation_i][sim_i] = sum(matches) / len(matches)
			proportion_complete = (sim_i+1) / n_sims
			stdout.write('\r')
			stdout.write(f"[{'=' * int(100 * proportion_complete):{100}s}] {int(100 * proportion_complete)}%")
			stdout.flush()
		stdout.write('\n')
	return results


if __name__ == '__main__':

	import argparse
	parser = argparse.ArgumentParser()
	parser.add_argument('factor', action='store', type=str, help='factor to simulate')
	parser.add_argument('output_dir', action='store', type=str, help='directory to write results to')
	parser.add_argument('--n_gradations', action='store', type=int, default=50, help='number of gradations in factor')
	parser.add_argument('--n_sims', action='store', type=int, default=100, help='number of simulations per gradation')
	args = parser.parse_args()

	results = simulate_factor(args.factor, args.n_gradations, args.n_sims)
	pickle(results, '%s/%s.pkl' % (args.output_dir, args.factor))
