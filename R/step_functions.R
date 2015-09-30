#' as_list
#'
#' A normal reduction step function that accumulates values to a list.
#' It also supports arity 0 and 1.
#' 
#' @param acc the accumulater, can be empty
#' @param input the input, can be empty
#' @return A step function that yields a list
#'
#' @export
as_list <- function(acc, input) {
  if (missing(acc) & missing(input)) {
    return(list())
  }
  if(missing(input)) {
    return(acc)
  }
  c(acc, list(input))
}

#' plus
#'
#' A normal reduction step function that sums over all items.
#' It also supports arity 0 and 1.
#' 
#' Deprecated.
#' 
#' @param acc the accumulater, can be empty
#' @param input the input, can be empty
#' @return A step function that yields a sum
#'
#' @export
plus <- function(acc, input) {
  warning("plus is deprecated. Please use as_sum.")
  as_sum(acc, input)
}

#' as_sum
#'
#' A normal reduction step function that sums over all items.
#' It also supports arity 0 and 1.
#' 
#' @param acc the accumulater, can be empty
#' @param input the input, can be empty
#' @return A step function that yields a sum
#'
#' @export
as_sum <- function(acc, input) {
  if (missing(acc) & missing(input)) {
    return(0)
  }
  if(missing(input)) {
    return(acc)
  }
  acc + input
}

#' as_vector
#'
#' Accumulates values to a vector by appending results
#' 
#' @param acc the accumulater, can be empty
#' @param input the input, can be empty
#' @return A step function that yields a vector
#'
#' @export
as_vector <- function(acc, input) {
  if (missing(acc) & missing(input)) {
    return(c())
  }
  if(missing(input)) {
    return(acc)
  }
  c(acc, input)
}


