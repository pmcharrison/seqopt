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
#' @param memoise Scalar Boolean;
#' whether or not the cost function should be memoised.
#' Defaults to \code{FALSE}, but enable this when the cost function
#' is time-consuming to compute and repeated transitions are anticipated.
#' @param vectorised Scalar Boolean;
#' whether or not \code{f} is vectorised.
#' If \code{f} is vectorised, it should take as its first input a
#' list of potential previous states,
#' with its second input being the new state, as before.
#' It should then return a numeric vector corresponding to the
#' transition cost associated with each potential previous state.
#' @return An object of class \code{cost_fun}, to be combined into a list
#' and passed to \code{seq_opt()}.
#' @export
cost_fun <- function(f,
                     context_sensitive,
                     vectorised = FALSE) {
  check_cost_fun(context_sensitive, f, vectorised)
  attr(f, "context_sensitive") <- context_sensitive
  attr(f, "vectorised") <- vectorised
  class(f) <- c("cost_fun", class(f))
  f
}

#' @export
is_vectorised <- function(x) {
  vectorised <- attr(x, "vectorised")
  if (!checkmate::qtest(vectorised, "B1"))
    stop("invalid 'vectorised' attribute")
  vectorised
}

#' @export
is_context_sensitive <- function(x) {
  context_sensitive <- attr(x, "context_sensitive")
  if (!checkmate::qtest(context_sensitive, "B1"))
    stop("invalid 'context_sensitive' attribute")
  context_sensitive
}

check_cost_fun <- function(context_sensitive, f, vectorised) {
  checkmate::qassert(context_sensitive, "B1")
  checkmate::qassert(vectorised, "B1")
  stopifnot(is.function(f))
  if (context_sensitive) {
    if (length(formals(f)) < 2L)
      stop("context-sensitive functions must take at least two arguments")
  } else {
    if (length(formals(f)) < 1L)
      stop("context-insensitive functions must take at least one argument")
  }
  invisible(TRUE)
}

#' Check for type 'cost function'
#'
#' Checks whether an object is a cost function.
#' @param x Object to check.
#' @return Logical scalar.
#' @export
is.cost_fun <- function(x) {
  is(x, "cost_fun")
}

print.cost_fun <- function(x, ...) {
  cat(sprintf("Context-%s ",
              if (is_context_sensitive(x)) "sensitive" else "free"))
  cat("cost function: \n")

  attributes(x) <- NULL
  print(x)
}
