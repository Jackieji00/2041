(* guards.ml: illustrate use of guards to further distinguish pattern
   matching cases *)

(* Count how many times elem appears in list *)
let rec count_occur elem list =
  match list with
  | [] -> 0
  | head :: tail ->       (* pattern doesn't compare head and elem *)
     if head=elem then    (* need an if/else to distinguish *)
       1 + (count_occur elem tail)
     else
       count_occur elem tail
;;

(* version that uses when guards *)
let rec count_occur elem list =
  match list with
  | [] -> 0
  | head :: tail when head=elem -> (* check equality in guard *)
     1 + (count_occur elem tail)
  | head :: tail ->                (* not equal, alternative *)
     count_occur elem tail
;;

(* Create a list of the elements between the indices start/stop in the
   given list. Uses a nested helper function for most of the work. *)
let elems_between start stop list =
  let rec helper i lst =
    if i > stop then
      []
    else if i < start then
      helper (i+1) (List.tl lst)
    else
      let first = List.hd lst in
      let rest =  List.tl lst in
      let sublst = helper (i+1) rest in
      first :: sublst
  in
  helper 0 list
;;


(* version of elems_between which uses match/with and when guards. *)
let elems_between start stop list =
  let rec helper i lst =
    match lst with
    | _         when i > stop  -> []
    | _ :: tail when i < start -> helper (i+1) tail
    | head :: tail             -> head :: (helper (i+1) tail)
    | _                        -> failwith "out of bounds" 
  in
  helper 0 list
;;       

(* (\* version of elems_between which uses match/with and when
 *    guards. This version will get warnings about potential pattern
 *    matching failure. *\)
 * let elems_between start stop list =
 *   let rec helper i lst =
 *     match lst with
 *     | _ when i > stop          -> []
 *     | _ :: tail when i < start -> helper (i+1) tail
 *     | head :: tail             -> head :: (helper (i+1) tail)
 *   in
 *   helper 0 list
 * ;; *)
