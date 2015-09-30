context("map")

test_that("map works on basic example", {
  result <- transduce(map(function(x) x + 1), as_sum, 1:5)
  expect_that(result, is_equivalent_to(sum(2:6)))
})

test_that("map_indexed works on basic example", {
  result <- transduce(map_indexed(function(i, x) i), as_sum, 10:20)
  expect_that(result, is_equivalent_to(sum(0:10)))
})