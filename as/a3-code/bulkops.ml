(* bulkops.ml: Implement bulk operations on Doccol's of string list
   Documents that are useful for multimanager.  Since the functions in
   this module require access to fields and types of other modules, start
   the file by opening those two modules:

   open Document;;
   open Doccol;;
*)
open Document;;
open Doccol;;

let showall doccol =
  let helper (name, doc)=
    Printf.printf "--List %s--\n" name;
    Sortedlist.print doc.current
  in List.iter (fun (n,d) -> helper (n,d)) doccol.docs
  ;;
(* val showall : string list Doccol.doccol -> unit
   Prints all documents in doccol to the screen. For each list,
   prints the list name first and then each element of the list using
   Sortedlist functions. Uses higher-order functions to iterate over
   the doclist.

   EXAMPLE:
   --List test-data/heros.txt--
   Asami
   Bolin
   Bumi
   Jinora
   Korra
   Kya
   Mako
   Tenzin

   --List test-data/villains.txt--
   Amon
   Hiroshi
   Kuvira
   Ming-Hua
   P-li
   Unalaq
   Zaheer

   --List default.txt--
   Korra
   Meelo
   Pema
*)

let saveall doccol =
  let helper (name,doc) =
    let lst = List.map (fun c-> c) doc.current in
    Util.strlist_to_file lst name in
  List.iter (fun (n,d) -> helper (n,d)) doccol.docs
;;
(* val saveall : string list Doccol.doccol -> unit
   Saves all documents in doccol. Makes use of Util functions to do
   I/O. Makes use of higher-order functions to write each list to
   associated file name. *)

let addall doccol elem =
  let helper (a,b) =
    let lst = Sortedlist.insert b.current elem in
    set b lst in
  List.iter (fun (a,b) -> (helper (a,b))) doccol.docs
;;
(* val addall : 'a list Doccol.doccol -> 'a -> unit
   Adds the given element to all docs in doccol. Makes use of
   higher-order functions and Sortedlist functions to modify each
   list. Each doc/list can individually undo the addition. *)

let mergeall doccol =
  List.fold_left (fun result (a,b) ->  Sortedlist.merge result b.current ) [] doccol.docs;;
(* val mergeall : 'a list Doccol.doccol -> 'a list
   Merges all lists in doccol.docs into a single list and returns
   it. Uses higher-order functions and Sortedlist functions to perform
   the merge. *)
