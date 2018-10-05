#' @export
seq_opt <- function(x, cost_funs = default_cost_funs()) {
  N <- length(x)

  # Element i of <costs> is a numeric vector,
  # the jth element of which corresponds to the
  # minimal (i.e. best) cost of the journey to x[[i]][[j]].
  costs <- vector(mode = "list", length = N)

  # Element i of <prev_states> is a numeric vector,
  # the jth element of which corresponds to the
  # predecessor to x[[i]][[j]] (i.e. a member of x[[i - 1]])
  # that achieves minimal cost.
  prev_states <- vector(mode = "list", length = N)

  if (N == 0) return(NULL)

  costs[[1L]] <- purrr::map_dbl(x[[1L]], function(y) {
    sum(costs(cost_funs = cost_funs, context = NULL, x = y))
  })
  prev_states[[1L]] <- rep(as.integer(NA), times = length(x[[1L]]))

  for (i in seq(from = 2L, length.out = N - 1L)) {
    prev_states[[i]] <- rep(as.integer(NA), times = length(x[[i]]))
    costs[[i]] <- rep(as.integer(NA), times = length(x[[i]]))
    for (j in seq_along(x[[i]])) {
      c(prev_states[[i]][[j]],
        costs[[i]][[j]]) %<-% best_prev_state(prev_state_values = x[[i - 1L]],
                                              prev_state_costs = costs[[i - 1L]],
                                              new_state_value = x[[i]][[j]],
                                              cost_funs = cost_funs)
    }
  }

  chosen_path <- rep(as.integer(NA), times = N)
  chosen_path[N] <- which.min(costs[[N]])
  chosen_cost <- costs[[N]][chosen_path[N]]

  for (i in seq(from = N - 1L, by = - 1L, length.out = N - 1L)) {
    chosen_path[i] <- prev_states[[i + 1L]][chosen_path[i + 1L]]
  }

  res <- purrr::map2(x, chosen_path, function(a, b) a[[b]])
  attr(res, "cost") <- chosen_cost
  res
}

best_prev_state <- function(prev_state_values,
                            prev_state_costs,
                            new_state_value,
                            cost_funs) {
  costs <- prev_state_costs +
    purrr::map_dbl(prev_state_values, function(prev_state_value) {
      sum(costs(cost_funs = cost_funs,
                context = prev_state_value,
                x = new_state_value))
    })
  i <- which.min(costs)
  list(best_prev_state = i, cost = costs[i])
}
