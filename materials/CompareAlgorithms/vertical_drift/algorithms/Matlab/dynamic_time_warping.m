%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dynamic Time Warping adapted from https://github.com/talcs/simpledtw
% This is used by the COMPARE and WARP algorithms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [dtw_cost, dtw_path] = dynamic_time_warping(sequence1, sequence2)

	n1 = size(sequence1, 1);
	n2 = size(sequence2, 1);
	dtw_cost = zeros(n1+1, n2+1);
	dtw_cost(1, :) = Inf;
	dtw_cost(:, 1) = Inf;
	dtw_cost(1, 1) = 0;
	for i = 1 : n1
		for j = 1 : n2
			this_cost = sqrt(sum((sequence1(i, :) - sequence2(j, :)).^2));
			dtw_cost(i+1, j+1) = this_cost + min([dtw_cost(i, j+1), dtw_cost(i+1, j), dtw_cost(i, j)]);
		end
	end
	dtw_cost = dtw_cost(2:n1+1, 2:n2+1);
	dtw_path(1:n1) = {{}};
	while i > 1 || j > 1
		dtw_path{i}{end+1} = j;
		possible_moves = [Inf, Inf, Inf];
		if i > 1 && j > 1
			possible_moves(1) = dtw_cost(i-1, j-1);
		end
		if i > 1
			possible_moves(2) = dtw_cost(i-1, j);
		end
		if j > 1
			possible_moves(3) = dtw_cost(i, j-1);
		end
		[~, best_move] = min(possible_moves);
		if best_move == 1
			i = i - 1;
			j = j - 1;
		elseif best_move == 2
			i = i - 1;
		else
			j = j - 1;
		end
	end
	dtw_path{1}{end+1} = 1;
	dtw_cost = dtw_cost(n1, n2);

end