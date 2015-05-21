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

#' map_indexed
#'
#' A map transducer that keeps an index. It starts with 0.
#' 
#' @param f a function that takes an index and an input and returns a value
#' @return A transducer that maps a value using f.
#'
#' @export
map_indexed <- function(f) {
  i <- -1
  map(function(input) {
    i <<- i + 1
    f(i, input)
  })
}

#' keep
#'
#' A filter transducer.
#' 
#' @param f a function that takes an input and returns TRUE or FALSE.
#' @return A transducer that filters a value using f.
#'
#' @export
keep <- function(f) {
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

#' random_sample
#'
#' Samples from the collection according to a probability.
#' 
#' @param prob a probability that an element is kept
#' @param seed a seed, optional.
#' @return A transducer that keeps elements according to a probability.
#'
#' @export
random_sample <- function(prob, seed = NULL) {
  if (prob < 0 || prob > 1) stop("prob should be between 0 and 1")
  has_seed <- !is.null(seed)
  keep(function(input) {
    if (has_seed) set.seed(seed)
    runif(1) <= prob
  })
}

#' discard
#'
#' Removes elements where the predicate is TRUE.
#' 
#' @param f a function that takes an input and returns TRUE or FALSE.
#' @return A transducer that discards a value using f.
#'
#' @export
discard <- function(f) keep(function(x) !f(x))


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

#' take
#'
#' Takes every nth element.
#' 
#' @param n indicates the what nth element.
#' @return A transducer that takes every nth element
#'
#' @export
take_nth <- function(n) {
  if (n < 1) {
    stop("n must be greater or equal to 1")
  }
  function(rf) {
    result_counter <- 0
    function(result = NULL, input = NULL) {
      if (is.null(result)) {
        return(rf())
      }
      if (is.null(input)) {
        return(rf(result))
      }
      result_counter <<- result_counter + 1
      if (result_counter == n) {
        result_counter <<- 0
        return(rf(result, input))
      } else {
        return(rf(result))
      }
    }
  }
}