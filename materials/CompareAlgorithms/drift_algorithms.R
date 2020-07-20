######################################################################
# ATTACH
######################################################################

attach <- function(fixation_XY, line_Y) {
	n <- nrow(fixation_XY)
	for (fixation_i in 1 : n) {
		fixation_y <- fixation_XY[fixation_i, 2]
		line_i <- which.min(abs(line_Y - fixation_y))
		fixation_XY[fixation_i, 2] <- line_Y[line_i]
	}
	return(fixation_XY)
}

######################################################################
# CHAIN
# 
# https://github.com/sascha2schroeder/popEye/
######################################################################

chain <- function(fixation_XY, line_Y, x_thresh=192, y_thresh=32) {
	n <- nrow(fixation_XY)
	dist_X <- abs(diff(fixation_XY[, 1]))
	dist_Y <- abs(diff(fixation_XY[, 2]))
	end_chain_indices <- which(dist_X > x_thresh | dist_Y > y_thresh)
	end_chain_indices <- append(end_chain_indices, n)
	start_of_chain <- 1
	for (end_of_chain in end_chain_indices) {
		mean_y <- mean(fixation_XY[start_of_chain:end_of_chain, 2])
		line_i <- which.min(abs(line_Y - mean_y))
		fixation_XY[start_of_chain:end_of_chain, 2] <- line_Y[line_i]
		start_of_chain <- end_of_chain + 1
	}
	return(fixation_XY)
}

######################################################################
# CLUSTER
# 
# https://github.com/sascha2schroeder/popEye/
######################################################################

cluster <- function(fixation_XY, line_Y) {
	n <- nrow(fixation_XY)
	m <- length(line_Y)
	fixation_Y <- fixation_XY[, 2]
	clusters <- kmeans(fixation_Y, m)
	ordered_cluster_indices <- order(clusters$centers)
	for (fixation_i in 1 : n) {
		cluster_i <- clusters$cluster[fixation_i]
		line_i <- which(ordered_cluster_indices == cluster_i)
		fixation_XY[fixation_i, 2] <- line_Y[line_i]
	}
	return(fixation_XY)
}

######################################################################
# COMPARE
#
# Lima Sanches, C., Kise, K., & Augereau, O. (2015). Eye gaze and text
#   line matching for reading analysis. In Adjunct proceedings of the
#   2015 ACM International Joint Conference on Pervasive and
#   Ubiquitous Computing and proceedings of the 2015 ACM International
#   Symposium on Wearable Computers (pp. 1227–1233). Association for
#   Computing Machinery.
#
# https://doi.org/10.1145/2800835.2807936
######################################################################

compare <- function(fixation_XY, word_XY, x_thresh=512, n_nearest_lines=3) {
	line_Y <- unique(word_XY[, 2])
	n <- nrow(fixation_XY)
	diff_X <- diff(fixation_XY[, 1])
	end_line_indices <- which(diff_X < -x_thresh)
	end_line_indices <- append(end_line_indices, n)
	start_of_line <- 1
	for (end_of_line in end_line_indices) {
		gaze_line <- fixation_XY[start_of_line:end_of_line,]
		mean_y <- mean(gaze_line[, 2])
		lines_ordered_by_proximity <- order(abs(line_Y - mean_y))
		nearest_line_I <- lines_ordered_by_proximity[1:n_nearest_lines]
		line_costs <- integer(n_nearest_lines)
		for (candidate_i in 1 : n_nearest_lines) {
			candidate_line_i <- nearest_line_I[candidate_i]
			text_line <- word_XY[word_XY[, 2] == line_Y[candidate_line_i],]
			dtw <- dynamic_time_warping(cbind(gaze_line[, 1]), cbind(text_line[, 1]))
			line_costs[candidate_i] <- dtw$cost
		}
		line_i <- nearest_line_I[which.min(line_costs)]
		fixation_XY[start_of_line:end_of_line, 2] <- line_Y[line_i]
		start_of_line <- end_of_line + 1
	}
	return(fixation_XY)
}

######################################################################
# MERGE
#
# Špakov, O., Istance, H., Hyrskykari, A., Siirtola, H., & Räihä,
#   K.-J. (2019). Improving the performance of eye trackers with
#   limited spatial accuracy and low sampling rates for reading
#   analysis by heuristic fixation-to-word mapping. Behavior Research
#   Methods, 51(6), 2661–2687.
#
# https://doi.org/10.3758/s13428-018-1120-x
# https://github.com/uta-gasp/sgwm
######################################################################

phases <- list(list('min_i'=3, 'min_j'=3, 'no_constraints'=FALSE), # Phase 1
               list('min_i'=1, 'min_j'=3, 'no_constraints'=FALSE), # Phase 2
               list('min_i'=1, 'min_j'=1, 'no_constraints'=FALSE), # Phase 3
               list('min_i'=1, 'min_j'=1, 'no_constraints'=TRUE))  # Phase 4

merge <- function(fixation_XY, line_Y, y_thresh=32, g_thresh=0.1, e_thresh=20) {
	n <- nrow(fixation_XY)
	m <- length(line_Y)
	diff_X <- diff(fixation_XY[, 1])
	dist_Y <- abs(diff(fixation_XY[, 2]))
	sequence_boundaries <- which(diff_X < 0 | dist_Y > y_thresh)
	sequence_boundaries <- append(sequence_boundaries, n)
	sequences <- list()
	start_of_sequence <- 1
	for (end_of_sequence in sequence_boundaries) {
		sequence <- list(start_of_sequence : end_of_sequence)
		sequences <- append(sequences, sequence)
		start_of_sequence <- end_of_sequence + 1
	}
	for (phase in phases) {
		while (length(sequences) > m) {
			best_merger <- c()
			best_error <- Inf
			for (i in 1 : (length(sequences)-1)) {
				if (length(sequences[[i]]) < phase$min_i)
					next # first sequence too short, skip to next i
				for (j in (i+1) : length(sequences)) {
					if (length(sequences[[j]]) < phase$min_j)
						next # second sequence too short, skip to next j
					candidate_XY <- fixation_XY[unlist(c(sequences[[i]], sequences[[j]])),]
					model <- lm(candidate_XY[,2] ~ candidate_XY[,1])
					gradient <- abs(coef(model)[2])
					error <- sqrt(sum(model$residuals^2) / nrow(candidate_XY))
					if (phase$no_constraints | (gradient < g_thresh & error < e_thresh)) {
						if (error < best_error) {
							best_merger <- c(i, j)
							best_error <- error
						}
					}
				}
			}
			if (length(best_merger) == 0)
				break # no possible mergers, break while and move to next phase
			merge_i <- best_merger[1] ; merge_j <- best_merger[2]
			merged_sequence <- list(c(sequences[[merge_i]], sequences[[merge_j]]))
			sequences <- append(sequences, merged_sequence)
			sequences[[merge_j]] <- NULL ; sequences[[merge_i]] <- NULL
		}
	}
	mean_Y <- integer(length(sequences))
	for (sequence_i in 1 : length(sequences))
		mean_Y[sequence_i] <- mean(fixation_XY[unlist(sequences[[sequence_i]]), 2])
	ordered_sequence_indices <- order(mean_Y)
	for (sequence_i in 1 : length(sequences)) {
		line_i <- which(ordered_sequence_indices == sequence_i)
		fixation_XY[unlist(sequences[[sequence_i]]), 2] <- line_Y[line_i]
	}
	return(fixation_XY)
}

######################################################################
# REGRESS
#
# Cohen, A. L. (2013). Software for the automatic correction of
#   recorded eye fixation locations in reading experiments. Behavior
#   Research Methods, 45(3), 679–683.
#
# https://doi.org/10.3758/s13428-012-0280-3
# https://blogs.umass.edu/rdcl/resources/
######################################################################

regress <- function(fixation_XY, line_Y, k_bounds=c(-0.1, 0.1), o_bounds=c(-50, 50), s_bounds=c(1, 20)) {
	n <- nrow(fixation_XY)
	m <- length(line_Y)

	fit_lines <- function(params, return_line_assignments=FALSE) {
		k <- k_bounds[1] + (k_bounds[2] - k_bounds[1]) * pnorm(params[1])
		o <- o_bounds[1] + (o_bounds[2] - o_bounds[1]) * pnorm(params[2])
		s <- s_bounds[1] + (s_bounds[2] - s_bounds[1]) * pnorm(params[3])
		predicted_Y_from_slope <- fixation_XY[, 1] * k
		line_Y_plus_offset <- line_Y + o
		density <- matrix(nrow=n, ncol=m)
		for (line_i in 1 : m) {
			fit_Y <- predicted_Y_from_slope + line_Y_plus_offset[line_i]
			density[, line_i] <- log(dnorm(fixation_XY[, 2], fit_Y, s))
		}
		if (return_line_assignments)
			return(apply(density, 1, which.max))
		return(-sum(apply(density, 1, max)))
	}

	best_fit <- optim(c(0, 0, 0), fit_lines)
	line_assignments <- fit_lines(best_fit$par, TRUE)
	for (fixation_i in 1 : n) {
		line_i <- line_assignments[fixation_i]
		fixation_XY[fixation_i, 2] <- line_Y[line_i]
	}
	return(fixation_XY)
}

######################################################################
# SEGMENT
#
# Abdulin, E. R., & Komogortsev, O. V. (2015). Person verification via
#   eye movement-driven text reading model, In 2015 IEEE 7th
#   International Conference on Biometrics Theory, Applications and
#   Systems. IEEE.
#
# https://doi.org/10.1109/BTAS.2015.7358786
######################################################################

segment <- function(fixation_XY, line_Y) {
	n <- nrow(fixation_XY)
	m <- length(line_Y)
	diff_X <- diff(fixation_XY[, 1])
	saccades_ordered_by_length <- order(diff_X)
	line_change_indices <- saccades_ordered_by_length[1:(m-1)]
	current_line_i <- 1
	for (fixation_i in 1 : n) {
		fixation_XY[fixation_i, 2] <- line_Y[current_line_i]
		if (is.element(fixation_i, line_change_indices))
			current_line_i <- current_line_i + 1
	}
	return(fixation_XY)
}

######################################################################
# SPLIT
######################################################################

split <- function(fixation_XY, line_Y) {
	n <- nrow(fixation_XY)
	diff_X <- diff(fixation_XY[, 1])
	clusters <- kmeans(diff_X, 2)
	sweep_marker <- which.min(clusters$centers)
	end_line_indices <- which(clusters$cluster == sweep_marker)
	end_line_indices <- append(end_line_indices, n)
	start_of_line <- 1
	for (end_of_line in end_line_indices) {
		mean_y <- mean(fixation_XY[start_of_line:end_of_line, 2])
		line_i <- which.min(abs(line_Y - mean_y))
		fixation_XY[start_of_line:end_of_line, 2] <- line_Y[line_i]
		start_of_line <- end_of_line + 1
	}
	return(fixation_XY)
}

######################################################################
# WARP
######################################################################

warp <- function(fixation_XY, word_XY) {
	n <- nrow(fixation_XY)
	dtw <- dynamic_time_warping(fixation_XY, word_XY)
	for (fixation_i in 1 : n) {
		words_mapped_to_fixation_i <- unlist(dtw$path[[fixation_i]])
		candidate_Y <- word_XY[words_mapped_to_fixation_i, 2]
		fixation_XY[fixation_i, 2] <- mode(candidate_Y)
	}
	return(fixation_XY)
}

mode <- function(values) {
	unique_values <- unique(values)
	return(unique_values[which.max(tabulate(match(values, unique_values)))])
}

######################################################################
# Dynamic Time Warping adapted from https://github.com/talcs/simpledtw
# This is used by the COMPARE and WARP algorithms
######################################################################

dynamic_time_warping <- function(sequence1, sequence2) {
	n1 <- nrow(sequence1)
	n2 <- nrow(sequence2)
	dtw_cost <- matrix(nrow=n1+1, ncol=n2+1)
	dtw_cost[1, ] <- Inf
	dtw_cost[ ,1] <- Inf
	dtw_cost[1,1] <- 0
	for (i in 1 : n1) {
		for (j in 1 : n2) {
			this_cost <- sqrt(sum((sequence1[i,] - sequence2[j,])^2))
			dtw_cost[i+1, j+1] <- this_cost + min(dtw_cost[i, j+1], dtw_cost[i+1, j], dtw_cost[i, j])
		}
	}
	dtw_cost <- dtw_cost[2:(n1+1), 2:(n2+1)]
	dtw_path <- replicate(n1, list())
	while (i > 1 | j > 1) {
		dtw_path[[i]] <- append(dtw_path[[i]], j)
		possible_moves <- c(Inf, Inf, Inf)
		if (i > 1 & j > 1)
			possible_moves[1] <- dtw_cost[i-1, j-1]
		if (i > 1)
			possible_moves[2] <- dtw_cost[i-1, j]
		if (j > 1)
			possible_moves[3] <- dtw_cost[i, j-1]
		best_move <- which.min(possible_moves)
		if (best_move == 1) {
			i <- i - 1
			j <- j - 1
		}
		else if (best_move == 2)
			i <- i - 1
		else
			j <- j - 1
	}
	dtw_path[[1]] <- append(dtw_path[[1]], 1)
	return(list('cost' = dtw_cost[n1, n2], 'path' = dtw_path))
}
