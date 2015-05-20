context("take")

test_that("take-nth takes every nth element.", {
  result <- transduce(take_nth(10), as_list, 1:20)
  expect_that(result, is_equivalent_to(list(10, 20)))
})

test_that("take-nth throws error if n < 1.", {
  expect_error(take_nth(0))
})
