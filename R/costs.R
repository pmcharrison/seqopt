#' Get cost by previous states
#'
#' Returns costs for moving to a given state from a vector of previous states.
#'
#' @param prev_state_values
#' List of potential previous states, over which this function is vectorized.
#'
#' @param new_state_value
#' State to move to.
#'
#' @param cost_funs
#' Named list of cost functions, each created by \code{\link{cost_fun}}.
#'
#' @param weights
#' Numeric vector of weights for these cost functions.
#'
#' @param exp_cost
#' (Logical scalar)
#' Whether the total costs should be exponentiated
#' (after summing over the cost functions).
#'
#' @param profile
#' (Logical scalar)
#' Whether to profile the time taken to compute each cost function.
#' If TRUE, profile information will be returned in the
#' "time" attribute of the output object.
#'
#' @return
#' A numeric vector of costs of the same length
#' as \code{prev_state_values},
#' corresponding to the cost associated with moving from
#' that previous state to the new state.
#'
#' @export
cost_by_prev_state <- function(prev_state_values,
                               new_state_value,
                               cost_funs,
                               weights,
                               exp_cost,
                               profile = FALSE) {
  n <- length(prev_state_values)
  x <- matrix(nrow = n, ncol = length(cost_funs)) # rows = previous states, cols = functions
  if (profile) {
    time <- rep(as.numeric(NA), times =  length(cost_funs))
    names(time) <- names(cost_funs)
  }
  for (i in seq_along(cost_funs)) { # iterate over columns / functions
    if (profile) start_time <- proc.time()
    f <- cost_funs[[i]]
    res <- if (is_context_sensitive(f)) {
      if (is_vectorised(f)) {
        as.numeric(f(prev_state_values, new_state_value))
      } else {
        purrr::map_dbl(prev_state_values, ~ f(., new_state_value))
      }
    } else {
      rep(as.numeric(f(new_state_value)),
          times = n)
    }
    if (length(res) != n) stop("cost function returned wrong number of outputs")
    if (anyNA(res)) stop("NA values not permitted in cost function output")
    res <- res * weights[i]
    x[, i] <- res
    if (profile) time[i] <- as.numeric(proc.time() - start_time)[3]
  }
  res <- rowSums(x) # linear predictor
  if (exp_cost) res <- exp(res) # exp(linear predictor)
  if (profile) attr(res, "time") <- time
  res
}

get_initial_costs <- function(x, cost_funs, weights, exp_cost, norm_cost, log_cost) {
  alphabet <- x[[1L]]
  res <- purrr::map_dbl(alphabet, function(val) {
    cost_by_fun <- purrr::map2_dbl(cost_funs, weights, function(f, weight) {
      function_output <- if (is_context_sensitive(f)) 0 else as.numeric(f(val))
      function_output * weight
    })
    sum(cost_by_fun)
  })
  if (exp_cost) res <- exp(res)
  if (norm_cost) res <- normalise(res)
  if (log_cost) res <- log(res)
  res
}
