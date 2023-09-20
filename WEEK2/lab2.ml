(* Defining types and functions explored in this lab*)
type
('xs, 'x0) foreach =
'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) rforeach =
'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0, 'r0) foldright =
'xs -> 'r0 -> ('x0 -> 'r0 -> 'r0) -> 'r0

let
string_length = String.length

let
string_get_at(cs:string)(i0:int): char = 
  String.get cs i0
;;

let foreach_to_foldleft(foreach: ('xs, 'x0) foreach): 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0 =
  fun (xs)(r0)(fopr) -> 
    let res = ref(r0) in
    foreach(xs)(fun(x0) -> res := fopr(!res)(x0)); 
    !res

let int1_foreach(n0:int)(work: int -> unit): unit =
  for i0 = 0 to n0-1 do 
    work(i0) 
  done

let int1_rforeach(n0:int)(work: int -> unit): unit =
  for i0 = 0 to n0-1 do 
    work(n0-1-i0) 
  done

let rforeach_to_foldright(rforeach: ('xs, 'x0) rforeach): ('xs, 'x0, 'r0) foldright =
  fun(xs)(r0)(fopr) ->
    let res = ref(r0) in
    rforeach(xs)(fun(x0) -> res := fopr(x0)(!res)); 
    !res

let int1_foldright(n0) =
  rforeach_to_foldright(int1_rforeach)(n0)

let int1_foldleft(n0) =
  foreach_to_foldleft(int1_foreach)(n0)

let rec list_revapp(xs: 'a list)(ys: 'a list): 'a list =
  match xs with
  | [] -> ys
  | x1 :: xs -> list_revapp(xs)(x1 :: ys)

let list_reverse(xs: 'a list): 'a list = 
  list_revapp(xs)([])

let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
  let work(x0) = 
    (res := (x0 :: !res)) 
  in(*let*)
  (fwork(work); list_reverse(!res))

let string_make_fwork( fwork: (char -> unit) -> unit): string =
  let xs =  Array.of_list(list_make_fwork(fwork)) in
  String.init (Array.length(xs)) (fun i -> xs.(i))
;;
(* end of types and functions *)

(* High Order Functions *)

(* 
  Why should you use high-order functions? High order functions allow a programmer to reuse code in different contexts.
   A generic function can be applied to many different problems that you might encounter.
   These high order functions also allow you to express what you are trying to do more eloquently and concisely.
   These functions are also easier to debug and maintain. You can pass in different arguments into these functions to test them and 
   easily find the root to your problems. Since you can apply these high order functions to many places in your code, it makes
   your code much cleaner, smoother, and easier to read.
*)

(* 
   Lets go through some examples of high order functions in OCaml.
*)

(* First, let's clarify what string_make_fwork does ...*)
let stringBuild = string_make_fwork(fun(work) -> work('a'); work('b'); work('c'))

(* What's going on? *)
(* This function looks extremely similar to String.init . That's because they are essentially doing the same thing.
   The difference is that this function uses a lambda function with a work function as a parameter, 
   while String.init uses a lambda function with an index as a parameter. Work takes in a character and places it into the final string 
   to build it, while String.init's lambda function takes in an index for which you specify what you want at it.
*)
(* A more complex example ...*)
let adder(xs: string): string = 
  let len = string_length(xs) in
  let rec index(ind: int)(work): unit = 
    if ind >= 0  then
      (work(string_get_at(xs)(ind));index(ind-1)(work))
    else
      ()
  in
  string_make_fwork(index(len-1))
;;
(* This above example just reverses the given string... You loop through the string backwards and add the current value to the final string.*)


(* Lets examine int1_foreach... *)
let ex1 = int1_foreach(5)(fun(x0) -> print_int(x0))

(* What is it doing? *)
(* It's parametized by the first argument (5), with a lambda function that takes in as a parameter the current value of the loop (x0). 
   At every single iteration, the current value of the loop is printed. The result is the printing of all the values from 0 to 5.

*)
(* Here's a function utilizing int1_foldleft... *)
let x = int1_foldleft(5)(0)(fun(r0)(x0) -> r0 + x0) 

(* What is it doing? *)
(* Think of it as a loop: It's parametized by the first argument (5, exclusive), with an accumulator defined by the second parameter (0), 
   and a lambda function that takes in as parameters the accumulator and the current value of the loop (r0 and x0). At every single iteration, 
   the current value of the loop is added to the accumulator. The result is the sum of all the values from 0 to 4. 
   The accumulator is returned at the end.
*)
(* So then, intuitively, what is int1_foldright? It essentially does the same thing, but from right to left. So in the case of an 
  integer, it would loop from 4 -> 0 instead of 0 -> 4. From above, int1_rforeach has the same intuition.
*)
let y = int1_foldright(5)(0)(fun(r0)(x0) -> r0 + x0) 
(* So a simple lambda function that sums up the values in that range will return the same value, whether you go left or right *)

(* Let's see a more complicated function using int1_foldleft... *)
let comp = int1_foldleft(5)(0)(fun(r0)(x0) -> r0 + x0 * int1_foldright(x0)(0)(fun(acc)(x) -> acc + x))
(* What is the above function doing?*)
(* 0x0 + 1x0 + 2x1 + 3x3 + 4x6 = 35*)

(* Instead of a verbose explanation, here's how the code would look like in Python:
  def comp():
    r0 = 0
    for x0 in range(5):
      acc = 0
      for x in range(x0, 0, -1):
        acc += x
      r0 += x0 * acc
    return r0
*)



