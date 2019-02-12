first_iter <- function(costs, x, cost_funs, weights, exp_cost, norm_cost, log_cost) {
  costs[[1L]] <- get_initial_costs(x,
                                   cost_funs,
                                   weights,
                                   exp_cost = exp_cost,
                                   norm_cost = norm_cost,
                                   log_cost = log_cost)
  costs
}
