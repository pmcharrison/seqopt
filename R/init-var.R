init_costs <- function(N) {
  # Element i of <costs> is a numeric vector,
  # the jth element of which corresponds to the
  # minimal (i.e. best) cost of the journey to x[[i]][[j]].
  costs <- vector(mode = "list", length = N)
}

init_best_prev_states <- function(N, x) {
  # Element i of <best_prev_states> is a numeric vector,
  # the jth element of which corresponds to the
  # predecessor to x[[i]][[j]] (i.e. a member of x[[i - 1]])
  # that achieves minimal cost.
  best_prev_states <- vector(mode = "list", length = N)
  if (N > 0)
    best_prev_states[[1L]] <- rep(as.integer(NA), times = length(x[[1L]]))
  best_prev_states
}
