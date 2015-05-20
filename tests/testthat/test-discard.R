context("discard")

test_that("discard filters out elements.", {
  result <- transduce(discard(function(x) x <= 19), plus, 1:20, init = 0)
  expect_that(result, is_equivalent_to(20))
})
