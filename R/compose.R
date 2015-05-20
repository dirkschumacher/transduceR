#' compose
#'
#' Composes a number of functions.
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