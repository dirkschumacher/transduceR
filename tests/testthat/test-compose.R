context("compose")

test_that("compose works with a single function", {
  result <- compose(function(x) x + 1)
  expect_that(result(4), is_equivalent_to(5))
})

test_that("compose works with a 5 functions", {
  result <- compose(function(x) x + 1, function(x) x + 1, 
                    function(x) x + 1, function(x) x + 1, 
                    function(x) x + 1)
  expect_that(result(1), is_equivalent_to(6))
})

test_that("compose throws error when no arguments are supplied", {
  expect_error(compose())
})