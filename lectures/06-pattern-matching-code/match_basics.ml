(* match_basics.ml: basic demo of pattern matching *)

(* Create a list comprised of the sum of adjacent pairs of
   elements in list. The last element in an odd-length list is
   part of the return as is. *)
let rec sum_adj_ie list =
  if list = [] then                   (* CASE of empty list *)
    []                                (* base case *)
  else
    let a = List.hd list in           (* DESTRUCTURE list *)
    let atail = List.tl list in       (* bind names *)
    if atail = [] then                (* CASE of 1 elem left *)
      [a]                             (* base case *)
    else                              (* CASE of 2 or more elems left *)
      let b = List.hd atail in        (* destructure list *)
      let tail = List.tl atail in     (* bind names *)
      (a+b) :: (sum_adj_ie tail)      (* recursive case *)
;;

(* pattern matching version: create a list with adjacent elements summed *)
let rec sum_adjacent list =
  match list with               (* case/destructure list separated by | *)
  | []             -> []        (* CASE of empty list *)
  | a :: []        -> [a]       (* CASE of 1 elem left *)
  | a :: b :: tail ->           (* CASE of 2 or more elems left *)
     (a+b) :: sum_adjacent tail
;;

(* # sum_adj_ie [1;2; 3;4; 5;6; 7];;
   - : int list = [3; 7; 11; 7]
   # sum_adj_ie [1;2; 3;4; 5;6; 7;8];;
   - : int list = [3; 7; 11; 15]
*)

(* # sum_adjacents [1;3;5;7;9;11];;
 * - : int list = [4; 12; 20]
 * # sum_adjacents [1;3;5;7;9;11;13];;
 * - : int list = [4; 12; 20; 13] *)

open Printf;;

(* Demonstrate conditional action using match/with *)
let yoda_say bool =
  match bool with
  | true  -> printf "False, it is not.\n"
  | false -> printf "Not true, it is.\n"
;;

(* Demonstrate conditional binding using match/with *)
let counsel mood =
  let message =                               (* bind message *)
    match mood with                           (* based on mood's value *)
    | "sad"      -> "Welcome to adult life"
    | "angry"    -> "Blame your parents"
    | "happy"    -> "Why are you here?"
    | "ecstatic" -> "I'll have some of what you're smoking"
    | s          -> "Tell me more about "^s   (* match any string *)
  in
  print_endline message;
;;

(* Both versions of these length functions work: pattern matching is
   "safe" in that it will not evaluate destruturing code that would
   throw exceptions.
*)
let rec length_A list =
  match list with
  | []           -> 0
  | head :: tail -> 1 + (length_A tail)
;;

let rec length_B list =
  match list with
  | head :: tail -> 1 + (length_B tail)
  | []           -> 0
;;

(* deeper destructuring of a list *)
let rec length_C list =
  match list with
  | []           -> 0
  | a :: []      -> 1
  | a :: b :: [] -> 2
  | head :: tail -> (length_C tail) + 1
;;

(* Swap adjacent elements in a list. If the list is odd length,
   the last element is dropped from the resulting list. *)
let rec swap_adjacent list =
  match list with
  | []             -> []            (* end of the line *)
  | a :: []        -> []            (* drop last elem *)
  | a :: b :: tail ->               (* two or more *)
     b :: a :: (swap_adjacent tail) (* swap order *)
;;

(* # swap_adjacent [1;2; 3;4; 5;6;];;
   - : int list = [2; 1; 4; 3; 6; 5]
   # swap_adjacent ["a";"b"; "c";"d"; "e"];;
   - : string list = ["b"; "a"; "d"; "c"]
   # swap_adjacent [];;
   - : 'a list = []
   # swap_adjacent [5];;
   - : int list = []
*)

(* Demonstrate some minor details of pattern matching *)
let cheap_counsel mood =
  match mood with
    "empty" ->                     (* first pipe | optional *)
     printf "Eat something.\n";
  | "happy" | "sad" | "angry" ->   (* multiple cases, same action *)
     printf "Tomorrow you won't feel '%s'\n" mood;
  | _ ->                           (* match anything, no binding *)
     printf "I can't help with that.\n";
;;

(* Arrays can be pattern matched but there is no size
   generalization like for lists *)
let array_size arr =
  match arr with
  | [||]          -> 0
  | [| _; |]      -> 1
  | [| _; _; |]   -> 2
  | [| _; _; _;|] -> 3
  | _ -> failwith "Too big, I give up"
;;
