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
as_list <- function(acc = NULL, input = NULL) {
  if (is.null(acc) & is.null(input)) {
    return(list())
  }
  if(is.null(input)) {
    return(acc)
  }
  c(acc, input)
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
plus <- function(acc = NULL, input = NULL) {
  if (is.null(acc) & is.null(input)) {
    return(0)
  }
  if(is.null(input)) {
    return(acc)
  }
  acc + input
}


