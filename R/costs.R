cost_by_prev_state <- function(prev_state_values, new_state_value, cost_funs) {
  n <- length(prev_state_values)
  x <- matrix(nrow = n, ncol = length(cost_funs))
  for (i in seq_along(cost_funs)) {
    f <- cost_funs[[i]]
    res <- if (f$context_sensitive) {
      f$fun(prev_state_values, new_state_value)
    } else {
      rep(f$fun(new_state_value), times = n)
    }
    if (length(res) != n) stop("cost function returned wrong number of outputs")
    if (!is.numeric(res)) stop("cost function did not return numeric outputs")
    x[, i] <- res
  }
  rowSums(x)
}

get_initial_costs <- function(x, cost_funs, norm_cost) {
  res <- purrr::map_dbl(x[[1L]], function(val) {
    sum(
      purrr::map_dbl(cost_funs, function(f) {
        if (f$context_sensitive) 0 else f$fun(val)
      }))
  })
  if (norm_cost) normalise(res) else res
}


