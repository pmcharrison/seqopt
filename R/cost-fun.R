cost_fun <- function(context_sensitive,
                     f,
                     weight = 1) {
  stopifnot(is.scalar.logical(context_sensitive),
            is.function(f),
            is.scalar.numeric(weight))
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
