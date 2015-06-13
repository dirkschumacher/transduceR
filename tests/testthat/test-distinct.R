context("distinct")

test_that("distinct removes duplicate elements", {
  result <- transduce(distinct(), plus, c(1, 2, 1, 1, 1), init = 0)
  expect_that(result, is_equivalent_to(3))
})

test_that("distinct works with other transducers", {
  transducer <- keep(function(x) x <= 4) %.% 
                        distinct() %.% 
                        take(4)
  result <- transduce(transducer, plus, c(1, 2, 1, 5, 1, 3, 4, 1, 1, 5, 6, 4), init = 0)
  expect_that(result, is_equivalent_to(1 + 2 + 3 + 4))
})
