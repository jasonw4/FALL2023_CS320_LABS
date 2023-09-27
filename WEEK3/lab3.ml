(* You know what combinators are, so here's combinators, but with lists *)
#use "./../MyOCaml.ml";;

(* This is code shown in lecture *)
let rec list_map(xs: 'a list)(fopr: 'a -> 'b): 'b list =
  match xs with
  | [] -> []
  | x1 :: xs -> fopr(x1) :: list_map(xs)(fopr)
;;

(* It essentially applies some lambda function to each element in the list. Quite straightforward. *)
 (* You can also refer to his lecture notes to see how this function can be implemented using combinators *)

 (* Some other popular list-combinators include zip and cross*)
  (* Zip essentially takes two lists and combines the elements at respective indices into tuples. So, [1;2;3] and [4;5;6] zipped would be 
     [(1, 4); (2, 5); (3, 6)]*)
 let rec zip(xs: 'a list)(ys: 'b list): ('a * 'b) list =
   match (xs, ys) with
   | ([], []) -> []
   | (x1 :: xs, y1 :: ys) -> (x1, y1) :: zip(xs)(ys)
   | _ -> raise (Failure "zip: lists have different lengths")

(* Cross is a bit more complicated. It takes two lists and returns a list of all possible combinations of elements from the two lists. So, [1;2;3] and [4;5;6] crossed would be
   [(1, 4); (1, 5); (1, 6); (2, 4); (2, 5); (2, 6); (3, 4); (3, 5); (3, 6)] *)
(* Not sure how he is going to implement cross and zip so for now just understand the concept *)


(* Options *)
(* Option types essentially ensure your function is going to return either None or Some. None can be seen as similar to 
   a null type. Some just means that you have some value you are returning, but just wrapped in an option type. So, while 4 is an int, 
   Some(4) will be of type int option. This gives you the opportunity to return null types and be a form of less verbose error handling. 
*)
