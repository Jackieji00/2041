(* partial_apply.ml: Demonstrate partial applications of functions *)

let add_on a b =                  (* standard function of 2 params *)     
  a + b                           
;;                                (* automatically curried *)
(* val add_on : int -> int -> int *)

let sevenA   = add_on 5 2;;       (* apply to 2 parameters *)
let elevenA  = add_on 5 6;;       (* result: int *)

let add_5A   = add_on 5;;         (* apply to 1 parameter *)
let add_9A   = add_on 9;;         (* result: int -> int *)

let sevenB  = add_5A 2;;          (* add_5 is a function of 1 parameter *)
let elevenB = add_5A 6;;          (* apply to 1 param results in int *)

let add_on_slam a =               (* standard function of 1 param *)
  (fun b -> a + b)                (* returns a function of 1 param *)
;;                                
(* val add_on_slam : int -> int -> int      same type as add_on *)

let sevenC = add_on_slam 5 2;;    (* apply just as 2 param version *)

let add_5B = add_on_slam 5;;      (* call with single param: curried *)
let sevenD = add_5B 2;;           (* call with additional parameter *)

(* Prior versions 'add_on' and 'add_on_slam' are internally converted
   to the version 'add_on_dlam' below. *)
let add_on_dlam =            (* bind name 'add_on_dlam' to ... *)
  (fun a ->                  (* a function of 1 param which returns... *)
    (fun b ->                (* a function of 1 param which returns... *)
      a+b))                  (* an answer through addition *)
;;                           
(* val add_on_dlam : int -> int -> int   same type as previous versions *)

let add_together (a,b) =          (* standard sytnax for function of 1 param: a pair *)
  a + b                           (* int * int -> int *)
;;                                (* first param is a pir of ints, not curried on them *)

let eightA = add_together (3,5);; (* must apply to a pair, no currying possible *)
