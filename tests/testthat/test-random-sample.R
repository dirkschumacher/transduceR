context("random_sample")

test_that("random_sample works with basic example.", {
  # not very good, maybe have a good stat. test 
  result <- transduce(random_sample(0.01), as_list, 1:1000)
  expect_that(length(result) <= 100, is_true())
})

test_that("random_sample only accepts probabilities", {
  # not very good, maybe have a good stat. test 
  expect_error(random_sample(-1))
  expect_error(random_sample(2))
})


test_that("random_sample accepts a seed", {
  # not very good, maybe have a good stat. test 
  result1 <- transduce(random_sample(0.3, 10), plus, 1:1000)
  result2 <- transduce(random_sample(0.3, 10), plus, 1:1000)
  expect_that(result1, is_equivalent_to(result2))
})

