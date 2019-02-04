# Returns a numeric vector of costs of the same length
# as prev_state_values,
# corresponding to the cost associated with moving from
# that previous state to the new state.
cost_by_prev_state <- function(prev_state_values,
                               new_state_value,
                               cost_funs,
                               exponentiate) {
  n <- length(prev_state_values)
  x <- matrix(nrow = n, ncol = length(cost_funs)) # rows = previous states, cols = functions
  for (i in seq_along(cost_funs)) { # iterate over columns / functions
    f <- cost_funs[[i]]
    res <- if (f$context_sensitive) {
      f$fun(prev_state_values, new_state_value)
    } else {
      rep(f$fun(new_state_value), times = n)
    }
    if (length(res) != n) stop("cost function returned wrong number of outputs")
    if (!is.numeric(res)) stop("cost function did not return numeric outputs")
    res <- res * f$weight
    x[, i] <- res
  }
  res <- rowSums(x) # linear predictor
  if (exponentiate) res <- exp(res) # exp(linear predictor)
  res
}

get_initial_costs <- function(x, cost_funs, norm_cost, exponentiate) {
  alphabet <- x[[1L]]
  res <- purrr::map_dbl(alphabet, function(val) {
    cost_by_fun <- purrr::map_dbl(cost_funs, function(f) {
      function_output <- if (f$context_sensitive) 0 else f$fun(val)
      f$weight * function_output
    })
    sum(cost_by_fun)
  })
  if (exponentiate) res <- exp(res)
  if (norm_cost) res <- normalise(res)
  res
}
