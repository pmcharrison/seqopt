context("test-summarise_problem")

test_that("examples", {
  x <- list(
    list(1, 2, 3),
    list(1, 2),
    list(1, 2, 3, 4, 5)
  )
  s <- summarise_problem(x)
  expect_equal(s$N, 3)
  expect_equal(s$num_alts, c(3, 2, 5))
  expect_equal(s$num_transitions, 3 * 2 + 5 * 2)
})
