costs <- function(cost_funs, context, x) {
  res <- rep(as.numeric(NA), times = length(cost_funs))
  for (i in seq_along(cost_funs)) {
    context_sensitive <- cost_funs[[i]]$context_sensitive
    f <- cost_funs[[i]]$f
    label <- cost_funs[[i]]$label
    res[i] <- if (context_sensitive) {
      if (is.null(context)) as.numeric(NA) else f(context, x)
    } else {
      f(x)
    }
    names(res)[i] <- label
  }
}

cost <- function(cost_funs, context, x) {
  sum(costs(cost_funs, x, context))
}
