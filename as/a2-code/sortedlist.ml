(* sortedlist.ml : Provides operations for sorted lists of any type. *)
open Printf;;
let rec insert list elem =
  match list with
  | [] -> elem :: list
  | head :: tail when elem < head -> elem:: head :: tail
  | head :: tail when elem = head -> head :: tail
  | head :: tail -> head :: (insert tail elem)
;;
(* val insert : 'a list -> 'a -> 'a list
   PROBLEM 1: Insert elem into list which is sorted.  The insertion
   preserves the sorted order of the list.  No duplicates are allowed
   in the list: if elem is equal to an element of list, the resulting
   list is identical to the original list. Uses pattern matching, not
   tail recursive. Runs in linear time on length of list.

  REPL EXAMPLES
  # insert [1;3;5;7] 8;;
  - : int list = [1; 3; 5; 7; 8]
  # insert [1;3;5;7] 2;;
  - : int list = [1; 2; 3; 5; 7]
  # insert [1;3;5;7] 5;;
  - : int list = [1; 3; 5; 7]
  # insert ["b";"d";"f"] "a";;
  - : string list = ["a"; "b"; "d"; "f"]
  # insert ["b";"d";"f"] "g";;
  - : string list = ["b"; "d"; "f"; "g"]
  # insert ["b";"d";"f"] "b";;
  - : string list = ["d"; "f"]
  # insert [] "g";;
  - : string list = ["g"]
*)

let rec remove list elem =
  match list with
  | [] -> []
  | head :: tail when head = elem -> tail
  | head :: tail -> head :: (remove tail elem)
;;
(* val remove : 'a list -> 'a -> 'a list
   PROBLEM 1: Create a new list with elem removed from the parameter
   list. If elem is not present in list, the result is identical to
   the original list. Uses pattern matching, not tail recursive.
   Runs in linear time on length of list.

   REPL EXAMPLES
   # remove [1;3;5;7] 1;
   - : int list = [3; 5; 7]
   # remove [1;3;5;7] 5;;
   - : int list = [1; 3; 7]
   # remove [1;3;5;7] 6;;
   - : int list = [1; 3; 5; 7]
   # remove ["b";"d";"f"] "b";;
   - : string list = ["d"; "f"]
   # remove ["b";"d";"f"] "z";;
   - : string list = ["b"; "d"; "f"]
*)

let rec print strlist =
  match strlist with
  | [] -> printf ""
  | head::tail -> printf "%s\n" head;
  print tail
;;
(* val print_strlist : string list -> unit
   PROBLEM 1: Print all elements of a string list to standard
   output. Makes use of standard printing functions such as
   print_endline or printf to print. Uses pattern matching. This
   function IS tail recursive. Runs in linear time on length of list.

   REPL EXAMPLES
   # print ["apple";"orange";"banana"];;
   apple
   orange
   banana
   - : unit = ()
   # print ["grape";"pear"];;
   grape
   pear
   - : unit = ()
*)
let rec merge lista listb =
  match lista,listb with
  | [],[] -> []
  | [],(hdb::tlb) -> listb
  | (hda::tla),[] -> lista
  | (hda::tla),(hdb::tlb) when hda= hdb -> hda::(merge tla tlb)
  | (hda::tla),(hdb::tlb) when hda< hdb -> hda::(merge tla listb)
  | (hda::tla),(hdb::tlb) -> hdb::(merge lista tlb)
;;
(* val merge : 'a list -> 'a list -> 'a list
   PROBLEM 2: Merge two sorted lists: lista and listb.  Elemetns that
   appear in both lists appear only once in the result.  Operates in
   linear time on the length of lists: does not do repeated
   insertion. Not tail recursive. May use pattern matching OR if/else
   clauses. Runs in linear time on combined length of lists.

   REPL EXAMPLES
   # merge [] [2;4;6];;
   - : int list = [2; 4; 6]
   # merge [1;3;5] [2;4;6];;
   - : int list = [1; 2; 3; 4; 5; 6]
   # merge [1;3;5] [];;
   - : int list = [1; 3; 5]
   # merge [1;3;5] [2;3;4;6;8];;
   - : int list = [1; 2; 3; 4; 5; 6; 8]
   # merge ["a";"c";"e"] ["b";"d"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"]
   # merge ["a";"c";"e"] ["b";"c";"d";"e"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"]
*)
