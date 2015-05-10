#' transducer
#' 
#' Implements transducers introduced in Clojure as well as infinite sequences.
#' 
#' @examples
#' transduce(take(5), plus_t, create_sequence(0, function(x) x + 1), init = 0)
#' @docType package
#' @name transducer
NULL