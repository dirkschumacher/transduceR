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
#' @param acc the accumulater, can be empty
#' @param input the input, can be empty
#' @return A step function that yields a sum
#'
#' @export
plus <- function(acc, input) {
  if (missing(acc) & missing(input)) {
    return(0)
  }
  if(missing(input)) {
    return(acc)
  }
  acc + input
}


