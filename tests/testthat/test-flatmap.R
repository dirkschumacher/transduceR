context("flatmap")

test_that("flat map works with basic example.", {
  result <- transduce(flat_map(function(x) c(-1, 1) * x), as_sum, 1:5)
  expect_that(result, is_equivalent_to(0))
})

