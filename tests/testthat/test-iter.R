context("iterators")

test_that("can use an iterator.", {
  result <- transduce(take(2), as_sum, iterators::icount(10))
  expect_that(result, is_equivalent_to(3))
})

test_that("does not produce warnings", {
  expect_silent(transduce(keep(function(x) x < 5), 
                          as_sum, iterators::icount(10)))
})
