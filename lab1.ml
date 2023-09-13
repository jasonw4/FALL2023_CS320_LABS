(* Use recursion instead of for-loops in Ocaml *)
(* Recursion is well suited for functional programming and aligns with its paradigms *)
(* This is due to several reasons ...
  1) You can create new values without modifying existing data (due to data being mostly immutable)
  2) Functions are prioritized in functional programming (as seen in the name), 
    you will see that the code you write in this class looks like a bunch of functions pieced together like a puzzle
  3) Stack space optimization, reusability, and the ability to build complex algorithms
*)
(*
  Here's a Python loop that increments a variable by 1, 10 times ...
  x = 0
  for i in range(10):
    x += 1
*)
(* the same python loop, defined as a recursive function in OCaml*)
let rec loop(x: int)(acc: int) = 
  if x = 10 then acc
  else loop(x + 1)(acc + 1)
;;

(* How different is OCaml syntax vs Python? *)
(*
  Fibonacci in Python: 
  def fibonacci(n: int) -> int:
    if n <= 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)
*)
(* Fibonacci in OCaml*)
let rec fib(n: int): int = 
  if n<=0 then 0 
  else if n=1 then 1 
  else fib(n-1) + fib(n-2)
;;

(* GCD function in Python: 
  recursively:
  def gcd(a, b):
    if b == 0:
      return a
    else:
      return gcd(b, a % b)
  iteratively:
  def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a
*)

(* GCD function on OCaml *)
let rec gcd (a: int)(b: int): int =
  if b = 0 then a
  else gcd(b)(a mod b)

(* previous factorial function, as shown in HW *)
let rec factorial(n: int): int =
  if n = 0 then 1
  else n * factorial (n - 1)
;;

(* tail recursive factorial function *)
let factorial_tail(n: int): int = 
  let rec fac_help(n: int)(acc: int): int =
    if n = 0 then acc
    else fac_help(n-1)(acc * n) 
  in
  fac_help(n)(1)
;;

(* tail recursive fibonacci *)
let fib_tail(n: int): int =
  let rec fib_help(n: int)(acc: int)(b: int): int = 
    if n = 0 then acc
    else fib_help(n-1)(b)(acc + b)
  in
    fib_help(n)(0)(1)
  ;;
    
(* you don't necessarily need the wrapper function but it is easier to pass in input this way *)
(* Why is tail recursion important? It decreases memory usage, and in return optimizes the performance*)

(* Mutual recursion is when two recursive functions call each other as its recursive case *)

let rec is_even (n: int): bool =
  if n = 0 then true
  else is_odd (n - 1)
and is_odd (n: int): bool =
  if n = 0 then false
  else is_even (n - 1)
;;