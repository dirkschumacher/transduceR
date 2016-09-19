context("contains")

test_that("contains stops when element is found.", {
  result <- transduce(contains(function(x) x == 10), as_sum, 
                      create_sequence(0, function(x) x + 1), init = 0)
  expect_that(result, is_equivalent_to(10))
})

test_that("contains continues the process on the found element", {
  pipe <- compose(contains(function(x) x == 10), map(function(x) x + 20))
  result <- transduce(pipe, as_sum, 1:20, init = 0)
  expect_that(result, is_equivalent_to(30))
})