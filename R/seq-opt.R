#' Find optimal sequence
#'
#' Given a list of timepoints and corresponding lists of possible states,
#' efficiently finds an optimal state sequence that minimises (or maximises)
#' an arbitrary transition cost function.
#' The implementation uses dynamic programming to achieve complexity
#' linear in the sequence length
#' and quadratic in the number of possible states.
#'
#' @param x A nested list describing the possible states at the possible time points.
#' Element \code{x[[i]]} should be a list describing the states available
#' at timepoint \code{i}.
#' Element \code{x[[i]][[j]]} should be the \code{j}th possible state
#' at timepoint \code{i}.
#'
#' @param cost_funs A list of cost functions,
#' with each cost function created by \code{cost_fun()}.
#' When applied to a state transition,
#' each cost function is computed, weighted by its weight parameter,
#' and summed to provide the total cost.
#' Decomposition of cost functions in this way has efficiency benefits
#' when some of the cost functions are context-independent
#' (i.e. the cost associated with moving to a state is independent of
#' the previous state).
#'
#' @param weights
#' Numeric vector of either length 1 or the same length as \code{cost_funs},
#' giving the weight for each cost function.
#'
#' @param verbose (Logical scalar)
#' Whether to display progress messages.
#'
#' @param exp_cost (Logical scalar)
#' Whether the combined cost function should be exponentiated.
#'
#' @param norm_cost (Logical scalar)
#' Whether or not the cost at each transition
#' (conditioned on the previous state)
#' should be normalised to sum to 1
#' for the set of possible continuations.
#' This yields a probabilistic interpretation of the cost function.
#' This takes place after the exponentiation controlled by the
#' \code{exp_cost} argument.
#'
#' @param log_cost
#' (Logical scalar)
#' Whether or not the final costs should have their logarithm taken.
#' This takes place after the normalisation controlled by
#' the \code{norm_cost} argument.
#'
#' @param minimise (Logical scalar)
#' Whether the cost function should be minimised or maximised.
#' @return A list where element \code{i} corresponds to the optimal
#' state at timepoint \code{i}.
#' @export
seq_opt <- function(x,
                    cost_funs,
                    weights = 1,
                    verbose = FALSE,
                    exp_cost = FALSE,
                    norm_cost = FALSE,
                    log_cost = FALSE,
                    minimise = TRUE) {
  check_inputs(cost_funs, weights, verbose, exp_cost, norm_cost, log_cost)
  if (length(weights) == 1L)
    weights <- rep(weights, length.out = length(cost_funs))

  if (verbose) summarise_problem(x)

  N <- length(x)
  if (N == 0) return(NULL)
  costs <- init_costs(N)
  best_prev_states <- init_best_prev_states(N, x)

  if (verbose) pb <- utils::txtProgressBar(max = N, style = 3)
  costs <- first_iter(costs, x, cost_funs, weights,
                      exp_cost = exp_cost,
                      norm_cost = norm_cost,
                      log_cost = log_cost)
  if (verbose) utils::setTxtProgressBar(pb, 1)

  for (i in seq(from = 2L, length.out = N - 1L)) {
    c(costs, best_prev_states) %<-% rest_iter(i, costs, x, cost_funs, weights,
                                              best_prev_states,
                                              exp_cost = exp_cost,
                                              norm_cost = norm_cost,
                                              log_cost = log_cost,
                                              minimise = minimise)
    if (verbose) utils::setTxtProgressBar(pb, i)
  }

  if (verbose) close(pb)
  find_path(x, costs, best_prev_states, N, minimise)
}

check_inputs <- function(cost_funs, weights, verbose, exp_cost, norm_cost, log_cost) {
  if (!is.list(cost_funs) ||
      !all(purrr::map_lgl(cost_funs, is.cost_fun)))
    stop("cost_funs must be a list of cost functions, ",
         "with each cost function created by cost_fun()")
  checkmate::qassert(weights, "N")
  checkmate::qassert(verbose, "B1")
  checkmate::qassert(exp_cost, "B1")
  checkmate::qassert(norm_cost, "B1")
  checkmate::qassert(log_cost, "B1")
  if (!(length(weights) == 1L || length(weights) == length(cost_funs)))
    stop("weights must have length of either 1 or length(cost_funs)")
}
