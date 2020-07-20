%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMPARE
%
% Lima Sanches, C., Kise, K., & Augereau, O. (2015). Eye gaze and text
%   line matching for reading analysis. In Adjunct proceedings of the
%   2015 ACM International Joint Conference on Pervasive and
%   Ubiquitous Computing and proceedings of the 2015 ACM International
%   Symposium on Wearable Computers (pp. 1227–1233). Association for
%   Computing Machinery.
%
% https://doi.org/10.1145/2800835.2807936
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fixation_XY = compare(fixation_XY, word_XY, x_thresh, n_nearest_lines)

	if ~exist('x_thresh', 'var')
		x_thresh = 512;
	end

	if ~exist('n_nearest_lines', 'var')
		n_nearest_lines = 3;
	end

	line_Y = unique(word_XY(:, 2));
	n = size(fixation_XY, 1);
	diff_X = diff(fixation_XY(:, 1));
	end_line_indices = find(diff_X < -x_thresh).';
	end_line_indices = [end_line_indices, n];
	start_of_line = 1;
	for end_of_line = end_line_indices
		gaze_line = fixation_XY(start_of_line:end_of_line, :);
		mean_y = mean(gaze_line(:, 2));
		[~, lines_ordered_by_proximity] = sort(abs(line_Y - mean_y));
		nearest_line_I = lines_ordered_by_proximity(1:n_nearest_lines);
		line_costs = zeros(1, n_nearest_lines);
		for candidate_i = 1 : n_nearest_lines
			candidate_line_i = nearest_line_I(candidate_i);
			text_line = word_XY(word_XY(:, 2) == line_Y(candidate_line_i), :);
			dtw_cost = dynamic_time_warping(gaze_line(:, 1), text_line(:, 1));
			line_costs(candidate_i) = dtw_cost;
		end
		[~, best_candidate_i] = min(line_costs);
		line_i = nearest_line_I(best_candidate_i);
		fixation_XY(start_of_line:end_of_line, 2) = line_Y(line_i);
		start_of_line = end_of_line + 1;
	end

end
