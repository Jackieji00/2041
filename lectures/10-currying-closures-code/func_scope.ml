(* func_scope.ml: functions retain the scopes/enviornment in which
   they are defined *)

open Printf;;

let x = "Mario";;
let print_player () =
  printf "%s\n" x;
;;

let x = "Luigi";;
print_player ();;

let e = ref "Bowser";;
let print_enemy () =
  printf "%s\n" !e;
;;

e := "Magikoopa";;
print_enemy ();;

let e = "Wario";;
print_enemy ();;

(* Commented version of the above *)

let x = "Mario";;            (* x is bound *)
let print_player () =        (* print_player uses *)
  printf "%s\n" x;           (* x, remember its value *)
;;

let x = "Luigi";;            (* rebind x to new value *)
print_player ();;            (* "Mario" : original x value is retained *)

let e = ref "Bowser";;       (* ref to string *)
let print_enemy () =         (* print_enemy uses value e *)
  printf "%s\n" !e;          (* remember its "value" : location for ref *)
;;

e := "Magikoopa";;           (* change ref *)
print_enemy ();;             (* "Magikoopa" printed *)

let e = "Wario";;            (* rebind e to "Wario" *)
print_enemy ();;             (* "Magikoopa" : uses original ref *)
