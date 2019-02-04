find_path <- function(x, costs, best_prev_states, N, minimise) {
  path <- rep(as.integer(NA), times = N)
  path[N] <- if (minimise) which.min(costs[[N]]) else which.max(costs[[N]])
  cost <- costs[[N]][path[N]]

  for (i in seq(from = N - 1L, by = - 1L, length.out = N - 1L)) {
    path[i] <- best_prev_states[[i + 1L]][path[i + 1L]]
  }

  res <- purrr::map2(x, path, function(a, b) a[[b]])
  attr(res, "cost") <- cost
  res
}
