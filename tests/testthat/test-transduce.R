context("transducer")

test_that("transducer works with sequences.", {
  result <- transduce(take(5), as_sum, create_sequence(0, function(x) x + 1), init = 0)
  expect_that(result, is_equivalent_to(sum(0:4)))
})

test_that("transducer works with vectors", {
  result <- transduce(take(5), as_sum, 1:10, init = 0)
  expect_that(result, is_equivalent_to(sum(1:5)))
})

test_that("transducer works with lists", {
  result <- transduce(take(5), as_sum, as.list(1:10), init = 0)
  expect_that(result, is_equivalent_to(sum(1:5)))
})

test_that("transducer works with no init value", {
  result <- transduce(transducer::map(function(x) x), as_sum, 1:3)
  expect_that(result, is_equivalent_to(sum(1:3)))
})

test_that("if map create multiple elements they are maintained by as_list", {
  result <- transduce(map(function(x) c(-1, 1) * x), as_list, 1:2)
  expect_that(length(result[[1]]), is_equivalent_to(2))
  expect_that(length(result[[2]]), is_equivalent_to(2))
})

test_that("transduce_tbl can iterate over rows by default", {
  result <- transduce_tbl(map(function(x) x$mpg * 10), as_sum, mtcars)
  expect_that(sum(mtcars$mpg * 10), is_equivalent_to(result))
})

test_that("transduce_tbl can iterate over columns", {
  result <- transduce_tbl(map(function(x) sum(x)), as_sum, mtcars, 
                          margin = "columns")
  expect_that(sum(mtcars), is_equivalent_to(result))
})

test_that("transduce_tbl can works with matrices", {
  result <- transduce_tbl(map(function(x) x[1] * 10), as_sum, as.matrix(mtcars))
  expect_that(sum(mtcars$mpg * 10), is_equivalent_to(result))
})

test_that("transduce_tbl fails if not used with a matrix or data.frame", {
  expect_error(transduce_tbl(map(function(x) x), as_sum, 1:10))
})