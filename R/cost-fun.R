#' Cost function
#'
#' Defines a cost function to be used in \code{seq_opt()}.
#' A cost function defines the penalty for a given state transitions.
#' If multiple cost functions are provided to \code{seq_opt()}
#' then costs are computed for each cost function and summed
#' to produce the final cost.
#' @param context_sensitive Scalar Boolean;
#' whether the function is affected by the identity of the previous state.
#' If \code{FALSE}, this means the cost function only depends on the identity
#' of the new state.
#' @param f Function defining the cost function.
#' If \code{context_sensitive = TRUE}, this function should take two arguments,
#' the first corresponding to the previous state,
#' and the second corresponding to the new state.
#' If \code{context_sensitive = FALSE}, this function should take one argument
#' corresponding to the new state.
#' @param weight Scalar numeric;
#' defines the multiplicative weight parameter for the cost function
#' when \code{seq_opt()} combines cost functions.
#' @return An object of class \code{cost_fun}, to be combined into a list
#' and passed to \code{seq_opt()}.
#' @export
cost_fun <- function(context_sensitive,
                     f,
                     weight = 1) {
  stopifnot(is.logical(context_sensitive),
            length(context_sensitive) == 1L,
            is.function(f),
            is.numeric(weight),
            length(weight) == 1L)
  x <- as.list(environment())
  class(x) <- "cost_fun"
  x
}

print.cost_fun <- function(x, ...) {
  cat("Cost function: \n")
  cat(sprintf("- context-%s\n",
              if (x$context) "sensitive" else "free"))
  cat(sprintf("- weight = %s\n", x$weight))
  print(x$f)
}
