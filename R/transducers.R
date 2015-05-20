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

#' discard
#'
#' Removes elements where the predicate is TRUE.
#' 
#' @param f a function that takes an input and returns TRUE or FALSE.
#' @return A transducer that discards a value using f.
#'
#' @export
discard <- function(f) filter(function(x) !f(x))


#' flat_map
#'
#' Maps each elements to a new domain and flattens the result
#' 
#' @param f a function that takes an input and returns a value
#' @return A transducer that maps a value and flattens the result.
#'
#' @export
flat_map <- function(f) compose(map(f), flatten())

#' flatten
#' 
#' Flattens a list or anything that works with length.
#' 
#' @export
flatten <- function() {
  function(rf) {
    function(result = NULL, input = NULL) {
      if (is.null(result)) {
        return(rf())
      } 
      if (is.null(input)) {
        return(rf(result))
      }
      tmp_result <- result
      for (x in input) {
        tmp_result <- rf(tmp_result, x)
      }
      return(tmp_result)
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
