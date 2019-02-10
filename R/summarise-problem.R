summarise_problem <- function(x) {
  N <- length(x)
  message("Optimising a sequence of length ",
          N %>% format(scientific = FALSE, big.mark = ","),
          ".")
  num_alts <- purrr::map_int(x, length)
  message("Each position has an average of ",
          mean(num_alts) %>% round(2) %>% format(scientific = FALSE, big.mark = ","),
          " alternatives.")
  num_transitions <- sum(purrr::map2_int(
    num_alts[- N], num_alts[- 1],
    function(context_n, next_n) context_n * next_n))
  message("The algorithm needs to calculate ",
          num_transitions %>% format(scientific = FALSE, big.mark = ","),
          " transition costs.")
  invisible(list(N = N, num_alts = num_alts, num_transitions = num_transitions))
}
