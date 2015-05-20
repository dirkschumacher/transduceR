is_reduced <- function(x) {
  !is.null(attr(x, "transducer_reduced"))
}

reduced <- function(x) {
  structure(x, transducer_reduced = TRUE)
}

reduce <- function(f, init, collection) {
  # no tail call optimization -> loop
  acc <- init
  for(i in 1:length(collection)) {
    acc <- f(acc, collection[[i]])
    if(is_reduced(acc)) {
      attributes(acc) <- NULL
      return(f(acc))
    }
  }
  return(f(acc))
}

reduce_sequence_eager <- function(f, init, collection) {
  # no tail call optimization -> loop
  acc <- init
  current_sequence <- collection
  while(TRUE) {
    current_head <- get_head(current_sequence)
    if (is.null(current_head)) {
      break
    }
    acc <- f(acc, current_head)
    if(is_reduced(acc)) {
      attributes(acc) <- NULL
      return(f(acc))
    }
    current_sequence <- rest(current_sequence)
  }
  f(acc)
}

#' Transduce
#'
#' This function is like reduce but it takes a transducer and a normal
#' step function used in reduce.
#' 
#' More documentation will follow. Api not stable yet.
#' 
#' @param transducer a transducer.
#' @param step an step function.
#' @param collection a collection or a sequence.
#' @param init an initial value. If ommitted the step function will be called to generate a value without an argument.
#' @return The result of the reduction piped through the transducer.
#'
#' @export
transduce <- function(transducer, step, collection, init = NULL) {
  fn <- transducer(step)
  i_val = init
  if(is.null(i_val)) {
    i_val = fn()
  }
  reducing_function <- if(class(collection) == "transducer_sequence") reduce_sequence_eager else reduce
  reducing_function(fn, i_val, collection)
}
