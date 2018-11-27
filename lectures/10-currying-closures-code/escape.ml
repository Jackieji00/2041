(* escape.ml: commentary on variable escape in afunc  *)
open Printf;;

let afunc paramX =              (* a function taking a paramter  *)
  let localA = "less" in        (* local variables *)
  let localB = "greater/eq" in
  let retfun paramY =           (* local function to be returned *)
    if paramX < paramY then     (* paramX "escapes" into retfun *)
      localA                    (* localA "escapes" into retfun *)
    else
      localB                    (* localB "escapes" into retfun *)
  in
  retfun                        (* return a function *)
;;

let res  = afunc 10 12;;        (* no need to save params/locals *)
(* val res : string = "less" *)

let gt10 = afunc 10;;           (* save paramX=10, etc somehow *)
(* val gt10 : int -> string *)
let gt42 = afunc 42;;           (* save paramX=42, etc *)
(* val gt42 : int -> string *)

let localA = "don't care!";;    (* has no effect on evaluation below *)

let res10_12 = gt10 12;;        (* use paramX=10, evaluate 10 < 12 *)
(* "less" *)
let res42_12 = gt42 12;;        (* use paramX=42, evaluate 42 < 12 *)
(* "greater/eq" *)
