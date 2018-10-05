costs <- function(cost_funs, context, x) {
  purrr::map_dbl(cost_funs, function(cost_fun) {
    res <- if (cost_fun$context_sensitive) {
      if (is.null(context)) 0 else cost_fun$f(context, x)
    } else {
      cost_fun$f(x)
    }
    if (length(res) != 1L || !is.numeric(res))
      stop("cost functions must return a scalar numeric output")
    res
  })
}
