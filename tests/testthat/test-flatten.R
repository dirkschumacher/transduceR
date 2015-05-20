context("flatten")

test_that("flatten works with standard lists.", {
  nested_list <- list(1, 2:4, 5:6, 7)
  result <- transduce(flatten(), plus, nested_list)
  expect_that(result, is_equivalent_to(sum(1:7)))
})
