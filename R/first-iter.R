first_iter <- function(costs, x, cost_funs, norm_cost, exponentiate) {
  costs[[1L]] <- get_initial_costs(x, cost_funs, norm_cost, exponentiate)
  costs
}
