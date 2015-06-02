#' compose
#'
#' Composes a number of functions. 
#' Evaluates functions from right to left.
#' 
#' @param ... functions that should be composed
#' @return The composition of the functions.
#'
#' @examples
#' composition <- compose(
#'    map(function(x) x + 1),
#'    keep(function(x) x %% 3 == 0),
#'    keep(function(x) x < 500),
#'    take(5)
#'  )
#' @export
compose <- function(...) {
  # without tail call optimization this does not make sense I guess
  fns <- list(...)
  if (length(fns) == 0) {
    stop("please provide at least one function")
  }
  compose_rec <- function(acc, f, fns) {
    combined_function <- function(...)acc(f(...))
    if (length(fns) == 0) {
      if (is.null(acc)) {
        return(f)
      }
      return(combined_function)
    }
    new_function <- fns[[1]]
    if (length(fns) > 1) {
      remaining_functions <- fns[-1]
    } else {
      remaining_functions <- list()
    }
    if (is.null(acc)) {
      return(compose_rec(f, new_function, remaining_functions))
    } 
    compose_rec(combined_function, new_function, remaining_functions)
  }
  compose_rec(NULL, fns[[1]], fns[-1])
}

#' composition operator
#'
#' a %.% b translates into compose(a, b). 
#' It should be used to design pipes based on transducers.
#' 
#' @param a a function that will be composed with function b.
#' @param b a function that takes the result of function b.
#' @return The composition of the functions.
#'
#' @examples
#' composition <- map(function(x) x + 1) %.%
#'    keep(function(x) x %% 3 == 0) %.%
#'    keep(function(x) x < 500) %.%
#'    take(5)
#' @rdname compose_operator
#' @export
`%.%` <- function(a, b) compose(a, b)
