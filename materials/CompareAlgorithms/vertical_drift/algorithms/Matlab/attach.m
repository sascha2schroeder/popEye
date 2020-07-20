%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ATTACH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fixation_XY = attach(fixation_XY, line_Y)

	n = size(fixation_XY, 1);
	for fixation_i = 1 : n
		fixation_y = fixation_XY(fixation_i, 2);
		[~, line_i] = min(abs(line_Y - fixation_y));
		fixation_XY(fixation_i, 2) = line_Y(line_i);
	end

end
