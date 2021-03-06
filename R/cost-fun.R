#' Cost function
#'
#' Defines a cost function to be used in \code{seq_opt()}.
#' A cost function defines the penalty for a given state transitions.
#' If multiple cost functions are provided to \code{seq_opt()}
#' then costs are computed for each cost function and summed
#' to produce the final cost.
#'
#' @param f Function defining the cost function.
#' If \code{context_sensitive = TRUE}, this function should take two arguments,
#' the first corresponding to the previous state,
#' and the second corresponding to the new state.
#' If \code{context_sensitive = FALSE}, this function should take one argument
#' corresponding to the new state.
#'
#' @param context_sensitive
#' (Logical scalar)
#' Whether the function is affected by the identity of the previous state.
#' If \code{FALSE}, this means the cost function only depends on the identity
#' of the new state.
#'
#' @param vectorised
#' (Logical scalar)
#' Whether or not \code{f} is vectorised.
#' If \code{f} is vectorised, it should take as its first input a
#' list of potential previous states,
#' with its second input being the new state, as before.
#' It should then return a numeric vector corresponding to the
#' transition cost associated with each potential previous state.
#'
#' @param symmetric
#' (Logical scalar)
#' Whether \code{f} is symmetric, meaning that it returns
#' the same result when transitioning forwards or backwards
#' between two states.
#'
#' @param has_reverse
#' (Logical scalar)
#' If \code{reverse == TRUE}, the cost function \code{f} should have an
#' optional argument \code{reverse} which defaults to \code{FALSE},
#' which when set to \code{TRUE} instructs the cost function
#' to compute the transition cost "backwards",
#' as if proceeding from the continuation to the context.
#'
#' @return An object of class \code{cost_fun}, to be combined into a list
#' and passed to \code{seq_opt()}.
#' @export
cost_fun <- function(f,
                     context_sensitive,
                     vectorised = FALSE,
                     symmetric = NA,
                     has_reverse = FALSE) {
  check_cost_fun(context_sensitive, f, vectorised, symmetric, has_reverse)
  if (vectorised && is.na(symmetric))
    stop("vectorised cost functions cannot have symmetric = NA")
  attr(f, "context_sensitive") <- context_sensitive
  attr(f, "vectorised") <- vectorised
  attr(f, "symmetric") <- symmetric
  attr(f, "has_reverse") <- has_reverse
  class(f) <- c("cost_fun", class(f))
  f
}

#' Is vectorised?
#'
#' Checks whether a cost function is vectorised.
#'
#' @param x Object to check.
#' @return Logical scalar.
#'
#' @export
is_vectorised <- function(x) {
  vectorised <- attr(x, "vectorised")
  if (!checkmate::qtest(vectorised, "B1"))
    stop("invalid 'vectorised' attribute")
  vectorised
}

#' Is context-sensitive?
#'
#' Checks whether a cost function is context-sensitive.
#'
#' @param x Object to check.
#' @return Logical scalar.
#'
#' @export
is_context_sensitive <- function(x) {
  context_sensitive <- attr(x, "context_sensitive")
  if (!checkmate::qtest(context_sensitive, "B1"))
    stop("invalid 'context_sensitive' attribute")
  context_sensitive
}

#' Is symmetric?
#'
#' Checks whether a cost function is symmetric.
#'
#' @param x Object to check.
#' @return Logical scalar.
#'
#' @export
is_symmetric <- function(x) {
  symmetric <- attr(x, "symmetric")
  if (!checkmate::qtest(symmetric, "b1"))
    stop("invalid 'symmetric' attribute")
  symmetric
}

#' Has reverse?
#'
#' Checks whether an object has an "as_reverse" attribute.
#'
#' @param x Object to check
#' @return Logical scalar.
#'
#' @export
has_reverse <- function(x) {
  has_reverse <- attr(x, "has_reverse")
  if (!checkmate::qtest(has_reverse, "B1"))
    stop("invalid 'has_reverse' attribute")
  has_reverse
}

check_cost_fun <- function(context_sensitive, f, vectorised,
                           symmetric, has_reverse) {
  checkmate::qassert(context_sensitive, "B1")
  checkmate::qassert(vectorised, "B1")
  checkmate::qassert(symmetric, "b1")
  checkmate::qassert(has_reverse, "B1")
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
