open Printf;;

(* match a pair and swap elements *)
let swap_pair (a,b) =
  let newpair = (b,a) in
  newpair
;;

(* 3 value kinds possible *)
type fruit = Apple | Orange | Grapes of int;;    

(* match a fruit  *)
let fruit_string f =
  match f with
  | Apple -> "you have an apple"
  | Orange -> "it's an orange"
  | Grapes(n) -> sprintf "%d grapes" n
;;

