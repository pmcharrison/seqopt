#' @export
seq_cost <- function(x, ...) {
  attr(seq_opt(lapply(x, list),
               ...),
       "cost")
}
