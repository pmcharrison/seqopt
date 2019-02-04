rest_iter <- function(i, costs, x, cost_funs, norm_cost,
                      best_prev_states, exponentiate) {
  # i = time point
  best_prev_states[[i]] <- rep(as.integer(NA), times = length(x[[i]]))
  costs[[i]] <- rep(as.integer(NA), times = length(x[[i]]))

  # cost_matrix:
  # j (rows): new_state
  # k (cols): previous state
  cost_matrix <- matrix(nrow = length(x[[i]]),
                        ncol = length(x[[i - 1]]))

  for (j in seq_along(x[[i]])) {
    cost_matrix[j, ] <- cost_by_prev_state(prev_state_values = x[[i - 1L]],
                                           new_state_value = x[[i]][[j]],
                                           cost_funs = cost_funs,
                                           exponentiate = exponentiate)
  }

  # If appropriate, normalise the cost matrix within each previous state
  # (esp. appropriate for probabilistic formulations)
  if (norm_cost)
    for (k in seq_len(ncol(cost_matrix)))
      cost_matrix[, k] <- normalise(cost_matrix[, k])

  # Add the accumulated costs for each previous state
  for (j in seq_len(nrow(cost_matrix)))
    cost_matrix[j, ] <- cost_matrix[j, ] + costs[[i - 1L]]

  # For each new state, identify which would be the best previous state
  for (j in seq_along(x[[i]])) {
    ind <- which.min(cost_matrix[j, ])
    best_prev_states[[i]][[j]] <- ind
    costs[[i]][[j]] <- cost_matrix[j, ind]
  }

  list(costs, best_prev_states)
}