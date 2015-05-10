# transduceR

A small package that implements Clojure's [transducers](http://blog.cognitect.com/blog/2014/8/6/transducers-are-coming) in R. It also has lazy sequence to work with infinite collection of items.

Transducers let you define functions like map and filter decoupled from the underlying list datastructure. If this is really usefl in R, I don´t quite know yet.

This package is currently in a pre-alpha state (very limited documentation and tests) and feedback is very welcome.

# Examples

## Transduce 
Only processes the first 10 items.
```R 
pipe <- compose(map(function(x) x + 1), take(10))
system.time(transduce(pipe, plus, 1:100000000))
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
