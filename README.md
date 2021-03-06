# Functional programming in R
[![Build Status](https://travis-ci.org/dirkschumacher/transduceR.png?branch=master)](https://travis-ci.org/dirkschumacher/transduceR)
[![Coverage Status](https://coveralls.io/repos/dirkschumacher/transduceR/badge.svg?branch=master)](https://coveralls.io/r/dirkschumacher/transduceR?branch=master)

A small package that implements Clojure's [transducers](http://clojure.org/transducers) in R. It also contains lazy sequences to work with infinite collection of items. 

Transducers let you define functions like map and filter decoupled from the underlying list datastructure. 

This package is currently in an alpha state and feedback is very welcome. Also the public API is far from ready and probably not optimal. I initially implemented it as an exercise but it might make sense to create a stable package out of it (in the future ;)).

Current version: 0.6.0

# Installation

To install the current development version use devtools:

```R 
devtools::install_github("dirkschumacher/transduceR")
```

# Quick start
```R 
library(transducer)
library(magrittr)
keep(function(x) x %% 7 == 0) %.%
  map(function(x) x + 1) %.%
  take(2) %>%
  transduce(as_sum, 1:100)
# 23

map(function(x) x %% 7) %.%
  distinct() %.%
  take(2) %>%
  transduce(as_list, 1:100)
  
# [[1]]
# [1] 1
#  
# [[2]]
# [1] 2

map(function(x) x$cyl) %.%
  distinct() %>%
  transduce_tbl(as_vector, mtcars)
# [1] 6 4 8

set.seed(2)
filter(function(x) x < 5) %.%
  take(3) %>%
  transduce(as_vector, iterators::irpois(lambda = 5, 10))
```

# API

## Reducing functions

* `transduce` takes a transducer, a step function and some collection and applies the transducers to it.
* `transduce_tbl` takes a transducer, a step function and a data.frame and applies the transducer to either to each column or each row.

## Transducers
All functions return transducers.

* `contains` takes a predicate and terminates the process if the predicate returns `TRUE`. See prime number example.
* `discard` filters out elements that satisfy the predicate
* `distinct` filters outs duplicate elements
* `keep` filters elements that satisfy a predicate
* `flatten` flattens a list/sequence
* `flat_map` maps and then flattens the result
* `map` takes a functions and applies that function to each element. 
* `map_indexed` map, but the first element is the index of the element.
* `random_sample` randomly samples according to some success probability (supports a seed).
* `take` stops the process after n elements
* `take_nth` takes every nth element
* `take_while` takes the predicate and stops the process when the predicate evaluates to `FALSE`.

## Step functions
* `as_sum` a normal `+` operation. However it accepts 1 and 0 arguments.
* `as_list` combines the resulting elements into a list
* `as_vector` combines the resulting elements into a vector

# Examples

## Transduce 
Takes the sum of the first 10 even numbers but it does not iterate through the full list; < 0.2 sec on my old mac.

```R 
transducer <- keep(function(x) x %% 2 == 0) %.% take(10)
col <- 1:100000000
system.time(transduce(transducer, plus, col))
```

Compute the same but as a list.

```R 
system.time(transduce(transducer, as_list, col))
```

## Lazy sequences 

```R 
natural_numbers <- create_sequence(0, function(x) x + 1) 
first_5_even <- transduce(compose(keep(function(x) x %% 2 == 0), take(5)), as_list, natural_numbers)
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

```R 
# find a prime
random_numbers <- create_sequence_from_function(function() floor(runif(1) * 10000)) 
is_prime <- function(x) x == 2 || x %% 2:floor(sqrt(x)) # elegant formulation by flodel (http://stackoverflow.com/users/1201032/flodel) http://stackoverflow.com/a/19767707
# returns 2 times the first prime number found
result <- transduce(compose(contains(is_prime), map(function(x) x * 2)), plus, random_numbers)
```

## Reuse transformations with different data structures
```R 
transformation <- compose(keep(function(x) x %% 2 == 0), take(10))
natural_numbers <- create_sequence(0, function(x) x + 1) 
some_vector <- 1:10000
a_list <- as.list(1:1000)

transduce(transformation, plus, natural_numbers)
transduce(transformation, plus, some_vector)
transduce(transformation, plus, a_list)
```


# Inspiration
Clearly this package has been inspired heavily by Clojure´s [transducers](http://clojure.org/transducers).


