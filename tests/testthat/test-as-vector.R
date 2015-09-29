context("step as_vector")

test_that("as_vector returns an int vector stops when element is found.", {
  result <- transduce(keep(function(x) x <= 10), as_vector, 1:20)
  expect_that(result, is_equivalent_to(1:10))
})

test_that("as_vector yields a vector.", {
  result <- transduce(take(4), as_vector, 
                      create_sequence(0, function(x) x + 1))
  expect_that(result, is_equivalent_to(c(0, 1, 2, 3)))
})
