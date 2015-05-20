context("transducer")

test_that("transducer works with sequences.", {
  result <- transduce(take(5), plus, create_sequence(0, function(x) x + 1), init = 0)
  expect_that(result, is_equivalent_to(sum(0:4)))
})

test_that("transducer works with vectors", {
  result <- transduce(take(5), plus, 1:10, init = 0)
  expect_that(result, is_equivalent_to(sum(1:5)))
})

test_that("transducer works with lists", {
  result <- transduce(take(5), plus, as.list(1:10), init = 0)
  expect_that(result, is_equivalent_to(sum(1:5)))
})

test_that("transducer works with no init value", {
  result <- transduce(transducer::map(function(x) x), plus, 1:3)
  expect_that(result, is_equivalent_to(sum(1:3)))
})

test_that("if map create multiple elements they are maintained by as_list", {
  result <- transduce(map(function(x) c(-1, 1) * x), as_list, 1:2)
  expect_that(length(result[[1]]), is_equivalent_to(2))
  expect_that(length(result[[2]]), is_equivalent_to(2))
})
