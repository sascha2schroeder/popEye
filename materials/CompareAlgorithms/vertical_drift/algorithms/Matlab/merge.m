%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MERGE
%
% Špakov, O., Istance, H., Hyrskykari, A., Siirtola, H., & Räihä,
%   K.-J. (2019). Improving the performance of eye trackers with
%   limited spatial accuracy and low sampling rates for reading
%   analysis by heuristic fixation-to-word mapping. Behavior Research
%   Methods, 51(6), 2661–2687.
%
% https://doi.org/10.3758/s13428-018-1120-x
% https://github.com/uta-gasp/sgwm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fixation_XY = merge(fixation_XY, line_Y, y_thresh, g_thresh, e_thresh)

	if ~exist('y_thresh', 'var')
		y_thresh = 32;
	end
	if ~exist('g_thresh', 'var')
		g_thresh = 0.1;
	end
	if ~exist('e_thresh', 'var')
		e_thresh = 20;
	end

	phases = [struct('min_i', 3, 'min_j', 3, 'no_constraints', false), ... % Phase 1
	          struct('min_i', 1, 'min_j', 3, 'no_constraints', false), ... % Phase 2
	          struct('min_i', 1, 'min_j', 1, 'no_constraints', false), ... % Phase 3
	          struct('min_i', 1, 'min_j', 1, 'no_constraints', true)];     % Phase 4

	n = size(fixation_XY, 1);
	m = length(line_Y);
	diff_X = diff(fixation_XY(:, 1));
	dist_Y = abs(diff(fixation_XY(:, 2)));
	sequence_boundaries = find(diff_X < 0 | dist_Y > y_thresh).';
	sequence_boundaries = [sequence_boundaries, n];
	sequences = {};
	start_of_sequence = 1;
	for end_of_sequence = sequence_boundaries
		sequence = start_of_sequence : end_of_sequence;
		sequences = [sequences, sequence];
		start_of_sequence = end_of_sequence + 1;
	end
	for phase = phases
		while length(sequences) > m
			best_merger = [];
			best_error = inf;
			for i = 1 : length(sequences)-1
				if length(sequences{i}) < phase.min_i
					continue; % first sequence too short, skip to next i
				end
				for j = i+1 : length(sequences)
					if length(sequences{j}) < phase.min_j
						continue; % second sequence too short, skip to next j
					end
					candidate_XY = fixation_XY([sequences{i}, sequences{j}], :);
					coefficients = polyfit(candidate_XY(:, 1), candidate_XY(:, 2), 1);
					residuals = candidate_XY(:, 2) - (coefficients(1) * candidate_XY(:, 1) + coefficients(2));
					g = abs(coefficients(1));
					e = sqrt(sum(residuals.^2) / size(candidate_XY, 1));
					if phase.no_constraints || (g < g_thresh && e < e_thresh)
						if e < best_error
							best_merger = [i, j];
							best_error = e;
						end
					end
				end
			end
			if isempty(best_merger)
				break; % no possible mergers, break while and move to next phase
			end
			merge_i = best_merger(1); merge_j = best_merger(2);
			merged_sequence = [sequences{merge_i}, sequences{merge_j}];
			sequences = [sequences, merged_sequence];
			sequences(merge_j) = []; sequences(merge_i) = [];
		end
	end
	mean_Y = zeros(1, length(sequences));
	for sequence_i = 1 : length(sequences)
		mean_Y(sequence_i) = mean(fixation_XY(sequences{sequence_i}, 2));
	end
	[~, ordered_sequence_indices] = sort(mean_Y);
	for sequence_i = 1 : length(sequences)
		line_i = find(ordered_sequence_indices == sequence_i);
		fixation_XY(sequences{sequence_i}, 2) = line_Y(line_i);
	end

end
