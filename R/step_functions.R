#' map
#'
#' A map transducer.
#' 
#' @param f a function that takes an input and returns a value
#' @return A transducer that maps a value using f.
#'
#' @export
map <- function(f) {
  function(rf) {
    function(result = NULL, input = NULL) {
      if (is.null(result)) {
        return(rf())
      }
      if (is.null(input)) {
        return(rf(result))
      }
      rf(result, f(input))
    }
  }
}

#' filter
#'
#' A filter transducer.
#' 
#' @param f a function that takes an input and returns TRUE or FALSE.
#' @return A transducer that filters a value using f.
#'
#' @export
filter <- function(f) {
  function(rf) {
    function(result = NULL, input = NULL) {
      if (is.null(result)) {
        return(rf())
      }
      if (is.null(input)) {
        return(rf(result))
      }
      if (f(input)) {
        return(rf(result, input))
      } else {
        rf(result)
      }
    }
  }
}

#' contains
#'
#' Checks if an element is in the sequence or not.
#' The process stopes when the element is contained in the sequence.
#' 
#' @param f a function that takes an input and returns TRUE or FALSE.
#' @return A transducer that checks for a value using f.
#'
#' @export
contains <- function(f) {
  function(rf) {
    function(result = NULL, input = NULL) {
      if (is.null(result)) {
        return(rf())
      }
      if (is.null(input)) {
        return(rf(result))
      }
      if (f(input)) {
        return(rf(reduced(rf(result, input))))
      } else {
        rf(result)
      }
    }
  }
}

#' take
#'
#' Takes x number of items from a process.
#' 
#' @param number_of_results a number indicating how many items this function should accept.
#' @return A transducer that stops after number_of_results items.
#'
#' @export
take <- function(number_of_results) {
  function(rf) {
    result_counter <- 0
    function(result = NULL, input = NULL) {
      if (is.null(result)) {
        return(rf())
      }
      if (is.null(input)) {
        return(rf(result))
      }
      if (result_counter < number_of_results) {
        result_counter <<- result_counter + 1
        return(rf(result, input))
      } else {
        return(rf(reduced(result)))
      }
    }
  }
}

#' take_while
#'
#' Take an item as long as a predicate is TRUE.
#' 
#' @param f a predicate indicating how long items should be processed.
#' @return A transducer that stops after f evaluates to FALSE.
#'
#' @export
take_while <- function(f) {
  function(rf) {
    function(result = NULL, input = NULL) {
      if (is.null(result)) {
        return(rf())
      }
      if (is.null(input)) {
        return(rf(result))
      }
      if (f(input)) {
        return(rf(result, input))
      } 
      return(rf(reduced(result)))
    }
  }
}

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


