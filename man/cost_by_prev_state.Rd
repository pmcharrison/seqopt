% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/costs.R
\name{cost_by_prev_state}
\alias{cost_by_prev_state}
\title{Get cost by previous states}
\usage{
cost_by_prev_state(prev_state_values, new_state_value, cost_funs, weights,
  exp_cost, profile = FALSE)
}
\arguments{
\item{prev_state_values}{List of potential previous states, over which this function is vectorized.}

\item{new_state_value}{State to move to.}

\item{cost_funs}{Named list of cost functions, each created by \code{\link{cost_fun}}.}

\item{weights}{Numeric vector of weights for these cost functions.}

\item{exp_cost}{(Logical scalar)
Whether the total costs should be exponentiated
(after summing over the cost functions).}

\item{profile}{(Logical scalar)
Whether to profile the time taken to compute each cost function.
If TRUE, profile information will be returned in the
"time" attribute of the output object.}
}
\value{
A numeric vector of costs of the same length
as \code{prev_state_values},
corresponding to the cost associated with moving from
that previous state to the new state.
}
\description{
Returns costs for moving to a given state from a vector of previous states.
}
