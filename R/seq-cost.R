#' Compute cost
#'
#' Computes the cost of a given sequence.
#'
#' @param x Sequence to analyze.
#' @param ... Further arguments to pass to \code{\link{seq_opt}}.
#'
#' @return Numeric scalar
#'
#' @export
seq_cost <- function(x, ...) {
  attr(seq_opt(lapply(x, list),
               ...),
       "cost")
}
