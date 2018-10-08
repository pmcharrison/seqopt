context("test-evals")

test_that("testing number of evals", {
  counter_context_free <- 0L
  counter_context_sens <- 0L
  cost_funs <- list(
    cost_fun(context_sensitive = FALSE, f = function(b) {
      counter_context_free <<- 1L + counter_context_free
      message("counter_context_free = ", counter_context_free)
      1
    }),
    cost_fun(context_sensitive = TRUE, f = function(a, b) {
      counter_context_sens <<- 1L + counter_context_sens
      message("counter_context_sens = ", counter_context_sens)
      1
    }))
  x <- list(
    as.list(1:5),
    as.list(1:5),
    as.list(1:5)
  )
  seq_opt(x, cost_funs)
  expect_equal(counter_context_free, 5 * 3)
  expect_equal(counter_context_sens, 2 * 5 * 5)
})
