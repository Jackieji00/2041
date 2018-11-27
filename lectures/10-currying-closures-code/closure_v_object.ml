(* closure_v_object.ml: Demonstrates how closures create "private"
   data that is only modifiable through function applications.  In
   that sense they are identical to the standard depiction of
   "objects" as having private data and state with associated
   function/methods to operate on them.  Note: defining objects via
   closures is a bit round-about and either use of records or the
   object system in OCaml is much preferred.  The below simply
   demonstrates the principle closure/object equivalnce. *)

open Printf;;

(* Create an "account" and return three functions to operate on the
   account *)
let make_account initial =
  let balance = ref initial in  (* local variable balance *)
  let current () =
    !balance                    (* balance escapes in function current *)
  in
  let deposit amount =
    balance := !balance+amount  (* and escapees in function deposit *)
  in
  let withdraw amount =         (* and escapes in function withdraw *)
    if amount <= (!balance) then
      balance := !balance-amount
    else
      failwith "Insufficient funds"
  in
  (current,deposit,withdraw)    (* return 3 closures (methods) to operate on the account *)
;;
    
(* Demonstrate the use of accounts *)

(* Create several closures all operating on the same ref *)
let (cur1,dep1,withd1) = make_account 100;;

(* Deposit, withdraw, etc. *)
dep1 100;;
printf "Current balance: %d\n" (cur1 ());;
withd1 128;;
printf "Current balance: %d\n" (cur1 ());;

(* New account (closures) and operations on it *)
let (cur2,dep2,withd2) = make_account 64;;
printf "Current balance: %d\n" (cur2 ());;
dep2 16;;
printf "Current balance: %d\n" (cur2 ());;
withd2 128;;                    (* exception *)
