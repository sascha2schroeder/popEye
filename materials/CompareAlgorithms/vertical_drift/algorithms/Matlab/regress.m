%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REGRESS
%
% Cohen, A. L. (2013). Software for the automatic correction of
%   recorded eye fixation locations in reading experiments. Behavior
%   Research Methods, 45(3), 679–683.
%
% https://doi.org/10.3758/s13428-012-0280-3
% https://blogs.umass.edu/rdcl/resources/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fixation_XY = regress(fixation_XY, line_Y, k_bounds, o_bounds, s_bounds)

	if ~exist('k_bounds', 'var')
		k_bounds = [-0.1, 0.1];
	end
	if ~exist('o_bounds', 'var')
		o_bounds = [-50, 50];
	end
	if ~exist('s_bounds', 'var')
		s_bounds = [1, 20];
	end

	n = size(fixation_XY, 1);
	m = length(line_Y);

	function [goodness_of_fit, line_assignments] = fit_lines(params)
		k = k_bounds(1) + (k_bounds(2) - k_bounds(1)) * normcdf(params(1));
		o = o_bounds(1) + (o_bounds(2) - o_bounds(1)) * normcdf(params(2));
		s = s_bounds(1) + (s_bounds(2) - s_bounds(1)) * normcdf(params(3));
		density = zeros(n, m);
		predicted_Y_from_slope = fixation_XY(:, 1).' * k;
		line_Y_plus_offset = line_Y + o;
		for line_i = 1 : m
			fit_Y = predicted_Y_from_slope + line_Y_plus_offset(line_i);
			density(:, line_i) = log(normpdf(fixation_XY(:, 2).', fit_Y, s));
		end
		[max_density, line_assignments] = max(density, [], 2);
		goodness_of_fit = -sum(max_density);
	end

	best_fit = fminsearch(@fit_lines, [0, 0, 0]); % Octave < 6.0 does not support handles to nested functions
	[~, line_assignments] = fit_lines(best_fit);
	for fixation_i = 1 : n
		line_i = line_assignments(fixation_i);
		fixation_XY(fixation_i, 2) = line_Y(line_i);
	end

end
