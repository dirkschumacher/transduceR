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

test_that("compose operator evaluates from right to left", {
  trans <- (function(x) x + 1) %.% 
    (function(y) y * 2) %.% 
    (function(y) y + 5)
  expect_that(trans(5), is_equivalent_to(21))
})

test_that("compose operator integrates with magrittr pipe and transducer", {
  library(magrittr)
  result <- map(function(x) x + 1) %.%
      keep(function(x) x %% 7 == 0) %.%
      take(5) %>%
      transduce(plus, 1:100)
  expect_that(result, is_equivalent_to(sum(c(7, 14, 21, 28, 35))))
})