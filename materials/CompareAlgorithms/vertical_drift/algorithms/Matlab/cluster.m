%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLUSTER
%
% https://github.com/sascha2schroeder/popEye/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fixation_XY = cluster(fixation_XY, line_Y)

	n = size(fixation_XY, 1);
	m = length(line_Y);
	fixation_Y = fixation_XY(:, 2);
	[clusters, centers] = kmeans(fixation_Y, m);
	[~, ordered_cluster_indices] = sort(centers);
	for fixation_i = 1 : n
		cluster_i = clusters(fixation_i);
		line_i = find(ordered_cluster_indices == cluster_i);
		fixation_XY(fixation_i, 2) = line_Y(line_i);
	end

end
