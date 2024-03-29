(* Return the value associated with query_key in association list alist.
   Raises a Not_found exception if there is no association *)
let rec assoc query_key alist =
  match alist with
  | [] -> raise Not_found
  | (key,value)::tail when query_key=key -> value
  | _::tail -> assoc query_key tail
;;

let alist1 = [(9,"nine");   (5,"five");    (2,"two");]
let alist2 = [("nine",3.0); ("five",2.24); ("two",1.41); ("six",2.45)];;

(* return a list with the given key/value added; if the key already
   exists, changes association to the new value *)
let rec add_assoc key value alist =
  match alist with
  | [] -> (key,value)::[]
  | (k,v) :: tail when key=k -> (key,value)::tail
  | (k,v) :: tail -> (k,v) :: (add_assoc key value tail)
;;

(* return a list with the given key and associated value removed; if
   the given key is not present, no change is made to the list. Does
   not raise exceptions. *)
(* let rec remove_assoc key alist =  *)
let rec remove_assoc key alist =
  match alist with
  | [] -> alist
  | (k,v) :: tail when key=k -> tail
  | (k,v) :: tail -> (k,v)::(remove_assoc key tail)
;;
