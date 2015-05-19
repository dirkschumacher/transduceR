#' creates a lazy sequence
#'
#' Create an (infinite) sequence
#' 
#' @param head_value the value of the first element
#' @param generator a function that takes the current head and produces a new item
#' @return A lazy sequence of items
#'
#' @examples
#' natural_numbers <- create_sequence(0, function(x) x + 1)
#' @export
create_sequence <- function(head_value, generator) {
  sequence <- structure(list(), current_head = head_value, 
                        generator = generator, class = "transducer_sequence")
  sequence
}

#' creates a lazy sequence from a arity-0 function
#'
#' Create an (infinite) lazy sequence
#' 
#' @param fn a function that returns a value
#' @return A lazy sequence created by calling fn whenver a new item is needed.
#'
#' @examples
#' random_numbers <- create_sequence_from_function(function() runif(1))
#' @export
create_sequence_from_function <- function(fn) create_sequence(fn(), function(x) fn())

#' Takes the next value of collection
#'
#' 
#' @param sequence a collection
#' @return the next element in the collection
#'
#' @examples
#' second_random_number <- next_value(create_sequence_from_function(function() runif(1)))
#' @export
next_value <- function(sequence) UseMethod("next_value")

#' returns the next value of a lazy sequence
#' @param sequence a sequence
#' 
#' @export
next_value.transducer_sequence <- function(sequence) {
  current_head = attr(sequence, "current_head", exact = TRUE)
  generator = attr(sequence, "generator", exact = TRUE)
  generator(current_head)
}

#' returns the next value of a list
#' 
#' I.e. the second value.
#' 
#' @param sequence a collection
#' 
#' @export
next_value.list <- function(sequence) if (length(sequence) >= 2) sequence[[2]] else NULL

#' Returns the rest of a collection
#' 
#' @param sequence a collection
#' 
#' That is everything except for the first item. 
#' Returns NULL if no more items exist.
#' @export
rest <- function(sequence) UseMethod("rest")

#' Returns the rest of a lazy sequence.
#' 
#' @param sequence a collection
#' 
#' @export
rest.transducer_sequence <- function(sequence) {
  generator = attr(sequence, "generator", exact = TRUE)
  next_value = next_value(sequence)
  if (is.null(next_value)) {
    return(NULL)
  }
  create_sequence(next_value, generator)
}

#' Returns the rest of a list
#' 
#' @param sequence a sequence
#' 
#' @export
rest.list <- function(sequence) {
  if (length(sequence) <= 1) {
    return(NULL)
  }
  sequence[2:length(sequence)]
}

#' Returns the first element of a collection.
#' @param sequence a sequence
#' @export
get_head <- function(sequence) UseMethod("get_head")

#' Returns the first element of a lazy sequence.
#' @param sequence a sequence
#' @export
get_head.transducer_sequence <- function(sequence) attr(sequence, "current_head", exact = TRUE)

#' Returns the first element of a list
#' @param sequence a list
#' @export
get_head.list <- function(sequence) sequence[[1]]

#' Converts a collection to a list
#' @param sequence a sequence
#' @export
to_list <- function(sequence) UseMethod("to_list")

#' Converts a lazy sequence to a list
#' @param sequence a list
#' @export
to_list.transducer_sequence <- function(sequence) reduce_sequence_eager(as_list, list(), sequence)
