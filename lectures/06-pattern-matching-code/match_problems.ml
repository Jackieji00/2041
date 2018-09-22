(* duplicate case "hi": second case not used *)
let opposites str =
  match str with
  | "hi" -> "bye"
  | "hola" -> "adios"
  | "hi" -> "oh god, it's you"
  | s -> s^" is it's own opposite"
;;

(* non-exhaustive matching: missing larger lists *)
let list_size list =
  match list with
  | [] -> "0"
  | a :: b :: [] -> "2"
  | a :: b :: c :: [] -> "3"
;;
