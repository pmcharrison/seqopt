context("test-main")

test_that("Prefer small numbers", {
  x <- lapply(1:10, function(x) 1:10)
  cost_funs <- list(cost_fun(FALSE, identity))
  res <- seq_opt(x, cost_funs)
  expect_equal(unlist(res), rep(1, times = 10))
})

test_that("Prefer large numbers", {
  x <- lapply(1:10, function(x) 1:10)
  cost_funs <- list(cost_fun(FALSE, function(x) - x))
  res <- seq_opt(x, cost_funs)
  expect_equal(unlist(res), rep(10, times = 10))
})

test_that("Must increase, prefer small changes", {
  x <- lapply(1:10, function(x) 1:10)
  cost_funs <- list(
    cost_fun(TRUE, function(a, b) if (b <= a) Inf else 0),
    cost_fun(TRUE, function(a, b) abs(b - a))
  )
  res <- unlist(seq_opt(x, cost_funs))
  expect_equal(res, 1:10)
})

test_that("Changing available states", {
  x <- lapply(1:10, function(x) 10 * x + 0:9)
  cost_funs <- list(
    # Difference between successive states should be divisible by 9
    cost_fun(TRUE, function(a, b) abs(((b - a) %% 9) - 0))
  )
  res <- unlist(seq_opt(x, cost_funs))
  expect_equal(res, c(10, 28, 37, 46, 55, 64, 73, 82, 91, 100))
})

test_that("Weight sensitivity", {
  x <- lapply(1:10, function(x) 1:10)
  cost_funs <- list(cost_fun(FALSE, function(x) x))
  seq_opt(x, cost_funs) %>%
    unlist() %>% expect_equal(rep(1, times = 10))
  seq_opt(x, cost_funs, weights = - 1) %>%
    unlist() %>% expect_equal(rep(10, times = 10))
})

test_that("Minimise versus maximise", {
  x <- lapply(1:10, function(x) 1:10)
  cost_funs <- list(cost_fun(FALSE, function(x) x))
  res <- seq_opt(x, cost_funs)
  expect_equal(unlist(res), rep(1, times = 10)) # prefers small numbers

  res <- seq_opt(x, cost_funs, minimise = FALSE)
  expect_equal(unlist(res), rep(10, times = 10)) # prefers large numbers
})
