# transduceR
[![Build Status](https://travis-ci.org/dirkschumacher/transduceR.png?branch=master)](https://travis-ci.org/dirkschumacher/transduceR)

A small package that implements Clojure's [transducers](http://blog.cognitect.com/blog/2014/8/6/transducers-are-coming) in R. It also contains lazy sequences to work with infinite collection of items. 

Transducers let you define functions like map and filter decoupled from the underlying list datastructure. 

This package is currently in a pre-alpha state (very limited documentation and tests) and feedback is very welcome. Also the public API is far from ready and probably not optimal. I initially implemented it as an exercise but it might make sense to create a package out of it.

# Installation

To install the current development version use devtools:

```R 
devtools::install_github("dirkschumacher/transduceR")
```

# Examples

## Transduce 
Take the sum of the first 10 even numbers. Does not process the rest of the list.
Transduce behaves almost like Reduce. However it also takes a transducer.

```R 
transducer <- compose(filter(function(x) x %% 2 == 0), take(10))
col <- 1:100000000
system.time(transduce(transducer, plus, col))
```

The same as a sum.

```R 
system.time(transduce(transducer, sum, col))
```

## Lazy sequences 

```R 
natural_numbers <- create_sequence(0, function(x) x + 1) 
first_5_even <- transduce(compose(filter(function(x) x %% 2 == 0), take(5)), as_list, natural_numbers)
```

```R 
sequence <- create_sequence_from_function(function() runif(1) * 100) # infinite random numbers
five_random_values <- transduce(take(5), as_list, sequence)
```

```R 
natural_numbers <- create_sequence(0, function(x) x + 1) 
get_head(natural_numbers) # 0
next_value(natural_numbers) # 1
rest(natural_numbers) # create_sequence(1, function(x) x + 1)
```

## Reuse transformations with different data structures
```R 
transformation <- compose(filter(function(x) x %% 2 == 0), take(10))
natural_numbers <- create_sequence(0, function(x) x + 1) 
some_vector <- 1:10000
a_list <- as.list(1:1000)

transduce(transformation, plus, natural_numbers)
transduce(transformation, plus, some_vector)
transduce(transformation, plus, a_list)
```


