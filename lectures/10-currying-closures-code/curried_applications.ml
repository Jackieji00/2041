open Printf;;
(* curried_applications.ml: define functions via partial applications *)

(* val print_greeting : string -> unit
   prints a greeting with format "Greetings, XXX: what flavor curry would you like?\n"
   with XXX filled in with a parameter string 

   # print_greeting "Elfo";;
   Greetings, Elfo: what flavor curry would you like? *)
let print_greeting =
  printf "Greetings, %s: what flavor curry would you like?\n"
;;

(* val sumlist : int list -> int     sum an a list of integers
   # sumlist [9;5;2];;
   - : int = 16 *)
let sumlist =
  List.fold_left (+) 0
;;

(* val divall : int -> int list -> int    Divide an integer by all integers in a list
   # divall 100 [2;5];;
   - : int = 10
   # divall 360 [5;6;4];;
   - : int = 3 *)
let divall =
  List.fold_left (/)
;;

(* val kaufenate : string list -> string list
   Prepend the string "Kauf" to a list of strings. Two curry opportunities.
   # kaufenate ["money"; "nix"; "tastic"];;
   - : string list = ["Kaufmoney"; "Kaufnix"; "Kauftastic"] *)
let kaufenate =
  List.map ((^) "Kauf")
;;
