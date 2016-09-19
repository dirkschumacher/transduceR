is_reduced <- function(x) {
  !is.null(attr(x, "transducer_reduced"))
}

reduced <- function(x) {
  structure(x, transducer_reduced = TRUE)
}

is_table <- function(x) {
  is.matrix(x) | is.data.frame(x)
}

reduce <- function(f, init, collection, margin = "columns") {
  # no tail call optimization -> loop
  acc <- init
  size_function <- length
  access_function <- `[[`
  if (is_table(collection)) {
    if (margin == "rows") {
      size_function <- nrow
      access_function <- function(tbl, row_idx) {
        tbl[row_idx, ]
      }
    } else {
      size_function <- ncol
    }
  }
  for(i in 1:size_function(collection)) {
    acc <- f(acc, access_function(collection, i))
    if(is_reduced(acc)) {
      attributes(acc) <- NULL
      return(f(acc))
    }
  }
  f(acc)
}

reduce_sequence_lazy <- function(f, init, collection, 
                                  head_fun = get_head, 
                                  replace_sequence = TRUE) {
  # no tail call optimization -> loop
  acc <- init
  current_sequence <- collection
  while(TRUE) {
    current_head <- tryCatch(head_fun(current_sequence), 
                             error = function(e) NULL)
    if (is.null(current_head)) {
      break
    }
    acc <- f(acc, current_head)
    if(is_reduced(acc)) {
      attributes(acc) <- NULL
      return(f(acc))
    }
    if (replace_sequence) {
      current_sequence <- rest(current_sequence)  
    }
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
#' @param step a step function.
#' @param collection a collection or a sequence.
#' @param init an initial value. If ommitted the step function will be called to generate a value without an argument.
#' @return The result of the reduction piped through the transducer.
#'
#' @export
transduce <- function(transducer, step, collection, init) {
  fn <- transducer(step)
  if(missing(init)) {
    i_val = fn()
  } else {
    i_val <- init
  }
  
  collection_class <- class(collection)
  reducing_function <- if ("transducer_sequence" %in% collection_class) {
    reduce_sequence_lazy
  } else if ("iter" %in% collection_class) {
    function(f, init, collection) {
      reduce_sequence_lazy(f, init, collection, 
                           iterators::nextElem, FALSE)
    } 
  } else {
    reduce
  }
    
  reducing_function(fn, i_val, collection)
}

#' Transduce_tbl
#'
#' Similiar to the normal transduce function but it explicitly only works for
#' data.frames or matrices. It has an additional parameter that indicates if
#' it should be iterated over columns or rows.
#' By default it iterates over the rows starting with the first row.
#' 
#' @param transducer a transducer.
#' @param step a step function.
#' @param tbl a table (matrix or data.frame).
#' @param margin indicates wether to iterate over "rows" or "columns". Default value is "rows".
#' @param init an initial value. If ommitted the step function will be called to generate a value without an argument.
#' @return The result of the reduction piped through the transducer.
#'
#' @export
transduce_tbl <- function(transducer, step, tbl, margin = "rows", 
                          init = transducer(step)()) {
  stopifnot(is_table(tbl))
  stopifnot(margin %in% c("rows", "columns"))
  reduce(transducer(step), init, tbl, margin)
}
