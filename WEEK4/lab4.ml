(* Lazy Evaluation and Streams *)
#use "./../MyOCaml.ml";;
(* What is Lazy Evaluation? *)
(* Lazy Evaluation is exactly what it sounds like: it computes values lazily. 
   This means that it will only compute values that are necessary. Why does this matter? Well, for very large input, trying to compute 
   all the values at once can lead to stack overflow. Computing values only when necessary leads to more efficient solutions.
*)

(* Streams are just structures that can be implemented by taking advantage of lazy evaluation. (Hongwei will implement his own version.)
   In class, he implements a bunch of stream functions, such as map, filter, foreach, etc. 
*)
(* Here's a simple function using streams: *)
let rec from n = StrCons(n, fun () -> from (n+1));;

(* This function takes in an integer and returns a stream of integers starting from that integer. 
   The second argument is a function that returns the rest of the stream. 
   This is how we can take advantage of lazy evaluation. 
   We can compute the rest of the stream only when we need it. 
   This is why we can have infinite streams.
*)

(* Here's another example of streams: *)
let rec fibonacci_stream () =
  let rec fib a b =
    StrCons(a, fun () -> fib b (a + b))
  in
  fib 0 1
(* This function returns a stream of fibonacci numbers. 
   The first two numbers are 0 and 1. 
   The rest of the stream is the sum of the first two numbers and the rest of the stream. 
   This is a recursive definition of the fibonacci sequence. 
*)
let StrCons(x1, xs) = fibonacci_stream()
let StrCons(x2, xs2) = xs();;

